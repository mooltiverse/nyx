/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mooltiverse.oss.nyx.command.template;

import java.lang.reflect.AnnotatedElement;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.Extension;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.TestTemplateInvocationContext;
import org.junit.jupiter.api.extension.TestTemplateInvocationContextProvider;
import org.junit.platform.commons.support.AnnotationSupport;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.command.Clean;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.command.Make;
import com.mooltiverse.oss.nyx.command.Mark;
import com.mooltiverse.oss.nyx.command.Publish;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.Script;
import com.mooltiverse.oss.nyx.git.Workbench;
import com.mooltiverse.oss.nyx.state.State;

/**
 * This is an invocation context provider to be used with test templates ({@link TestTemplate}) to produce invocation
 * contexts for such templates and resolve additional annotations and parameters to be used as fixtures.
 * See the <a href="https://junit.org/junit5/docs/current/user-guide/#extensions">Gradle Extension Model</a> documentation for more.
 * <br>
 * With more details, this provider produces two contexts: one to run commands standalone, the other to test them
 * invoked from the {@link Nyx} class. This means that each template is executed twice, one for each context, without duplicating test code.
 * <br>
 * Moreover, this extension can inject common entities as method parameters so that you don't need to repeat the same
 * initialization code for all tests. The type of parameters that can be injected are {@link CommandProxy} (or {@link Command}),
 * {@link Script} and {@link Workbench}.
 * <br>
 * In order to use this in templates you need to register this provider as a test extension declaratively in a test method,
 * like:<br>
 * <pre>
 *   @TestTemplate
 *   @ExtendWith(CommandInvocationContextProvider.class)
 *   public void myTest(...) {
 *      ...
 *   }
 * </pre>
 * <br>
 * or at the class level, like:<br>
 * <pre>
 *   @ExtendWith(CommandInvocationContextProvider.class)
 *   public class MyTestClass {
 *      ...
 *   }
 * </pre>
 * <br>
 * This way you can have all of the fixture parameters properly injected, if you declare them.
 * Remember that the test method must use the {@link TestTemplate} annotation.
 * <br>
 * Parameters that can be resolved by this extension are:<br>
 * - {@link Command} or {@link CommandProxy}: whenever you declare a parameter of type {@link Command}
 *   or {@link CommandProxy} in a test parameter a proxy to a specific Nyx command is injected.
 *   Although this is a standard interface, the class implementing the injected parameter depends
 *   on the context so it may be a standalone command or a wrapper that invokes the command specific
 *   business method on the {@link Nyx} class.
 *   Please note that {@link Command} parameters will actually be {@link CommandProxy} objects
 *   that are safely casted because {@link CommandProxy} inherits from {@link Command}.
 *   In order for this provider to be able to select the right command for the proxy,
 *   the {@link CommandSelector} annotation must be present on the specific injected method
 *   parameter or on the test method. Moreover, the {@link Baseline} annotation must also be
 *   applied on the parameter or the method or the declaring class to let the provider know
 *   the desired state of the Git repository that the command needs to run in.
 * - {@link Script} and {@link Workbench}: when you declare a parameter of that type you receive
 *   the object injected, with its status defined by the {@link Baseline} annotation, which must
 *   be present
 * <br>
 * The {@link Baseline} can be shared when applied to methods or classes. In this case they will
 * yield to the same objects, unless overridden in more specific contexts. For example, if you apply
 * it on a method and declare two parameters of type {@link CommandProxy} (or {@link Command}) and
 * {@link Script} in the method they will both yield use the same temporary directory that has been
 * created with a Git repository whose initial state depends on the {@link Baseline} annotation. Example:<br>
 * <pre>
 *   @TestTemplate
 *   @ExtendWith(CommandInvocationContextProvider.class)
 *   @Baseline(Scenario.INITIAL_COMMIT)
 *   public void myTest(@CommandSelector(Commands.PUBLISH) CommandProxy command, Script script) {
 *      ...
 *   }
 * </pre>
 * will have the {@code command} running the {@code publish} command in a directory with a Git repository
 * initialized with just an initial commit. The {@code script} parameter will be using the same Git
 * directory.
 * 
 * @see TestTemplate
 */
public class CommandInvocationContextProvider implements TestTemplateInvocationContextProvider {
    /**
     * Default constructor
     */
    public CommandInvocationContextProvider() {
        super();
    }

    /**
     * Returns {@code true} if this provider can provide invocation contexts for the given context.
     * 
     * @return {@code true} always.
     */
    @Override
    public boolean supportsTestTemplate(ExtensionContext context) {
        return true;
    }

    /**
     * Returns a stream with two invocation contexts, one for standalone command invocation, the other for invoking
     * the command through the {@link Nyx} class.
     * 
     * @return a stream with two invocation contexts, one for standalone command invocation, the other for invoking
     * the command through the {@link Nyx} class.
     */
    @Override
    public Stream<TestTemplateInvocationContext> provideTestTemplateInvocationContexts(ExtensionContext context) {
        return Stream.<TestTemplateInvocationContext>of(new StandaloneCommandInvocationContext(context.getDisplayName()), new NyxCommandInvocationContext(context.getDisplayName()));
    }

    /**
     * The context used to invoke commands as standalone objects. This context resolves parameters so that commands
     * are executed standalone.
     */
    public static class StandaloneCommandInvocationContext implements TestTemplateInvocationContext {
        /**
         * The display name passed in the constructor and also returned by {@link #getDisplayName(int)}.
         */
        private String displayName = null;

        /**
         * Standard constructor.
         * 
         * @param displayName the display name to be returned when {@link #getDisplayName(int)} is invoked.
         */
        StandaloneCommandInvocationContext(String displayName) {
            super();
            this.displayName = displayName;
        }

        /**
         * Returns the display name for this invocation. Since only one context is returned for each template,
         * this method ignores the given {@code invocationIndex} and returns the display name passed in the
         * constructor, just prefixed by a string to denote this kind of context.
         * 
         * @param invocationIndex the index of this execution
         * 
         * @return the display name for this invocation
         */
        @Override
        public String getDisplayName(int invocationIndex) {
            return "[context: ".concat(StandaloneCommandProxy.CONTEXT_NAME).concat("] ").concat(Objects.isNull(displayName) ? Integer.toString(invocationIndex) : displayName);
        }

        /**
         * Returns the list of additional extensions (parameter resolvers) for this invocation.
         * 
         * @return the list of additional extensions (parameter resolvers) for this invocation.
         */
        @Override
        public List<Extension> getAdditionalExtensions() {
            return List.<Extension>of(new StandaloneCommandParameterResolver(), new BaselineParameterResolver());
        }
    }

    /**
     * The context used to invoke commands using the {@link Nyx} class. This context resolves parameters so that commands
     * are executed as business methods on the main {@link Nyx} class. 
     */
    public static class NyxCommandInvocationContext implements TestTemplateInvocationContext {
        /**
         * The display name passed in the constructor and also returned by {@link #getDisplayName(int)}.
         */
        private String displayName = null;

        /**
         * Standard constructor.
         * 
         * @param displayName the display name to be returned when {@link #getDisplayName(int)} is invoked.
         */
        public NyxCommandInvocationContext(String displayName) {
            super();
            this.displayName = displayName;
        }

        /**
         * Returns the display name for this invocation. Since only one context is returned for each template,
         * this method ignores the given {@code invocationIndex} and returns the display name passed in the
         * constructor, just prefixed by a string to denote this kind of context.
         * 
         * @param invocationIndex the index of this execution
         * 
         * @return the display name for this invocation
         */
        @Override
        public String getDisplayName(int invocationIndex) {
            return "[context: ".concat(NyxCommandProxy.CONTEXT_NAME).concat("       ] ").concat(Objects.isNull(displayName) ? Integer.toString(invocationIndex) : displayName);
        }

        /**
         * Returns the list of additional extensions (parameter resolvers) for this invocation.
         * 
         * @return the list of additional extensions (parameter resolvers) for this invocation.
         */
        @Override
        public List<Extension> getAdditionalExtensions() {
            return List.<Extension>of(new NyxCommandParameterResolver(), new BaselineParameterResolver());
        }
    }

    /**
     * The common superclass for parameter resolvers implemented by this provider.
     */
    public static abstract class AbstractParameterResolver implements ParameterResolver {
        /**
         * The type of the parameter resolved by the resolver.
         */
        private final Class<?> parameterType;

        /**
         * Constructor.
         * 
         * @param parameterType the type of the parameter resolved by the resolver. If more types are resolved but they
         * have a common superclass, pass the superclass here.
         */
        protected AbstractParameterResolver(Class<?> parameterType) {
            super();
            this.parameterType = parameterType;
        }

        /**
         * Returns {@code true} the parameter to be resolved from the given context is assignable from the type
         * this resolver is built for (the type passed in the constructor).
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return {@code true} the parameter to be resolved from the given context is assignable from the type
         * this resolver is built for (the type passed in the constructor).
         */
        @Override
        public boolean supportsParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            return parameterContext.getParameter().getType().isAssignableFrom(parameterType);
        }

        /**
         * Resolves the {@link Baseline} annotation relatively to the given scope by returning a previously stored instance of the
         * script relatively to the given scope. If no previous instance is found, a new one is created and stored for further use.
         * 
         * The annotation is resolved and shared relatively to the given scope so that, once it's resolved for the first time,
         * further invocations using the same scope can return the same object.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * @param scope the scope the parameter is resolved against
         * @param scopeId the unique string identifier of the given scope
         * 
         * @return the resolved {@link Script} object, never {@code null}
         * 
         * @throws ParameterResolutionException if the parameter cannot be resolved
         */
        protected Script resolveSharedBaseline(ParameterContext parameterContext, ExtensionContext extensionContext, AnnotatedElement scope, String scopeId)
            throws ParameterResolutionException {
            try {
                ExtensionContext.Store store = extensionContext.getStore(ExtensionContext.Namespace.create(scopeId));
                Script value = store.get("baseline", Script.class);
                if (Objects.isNull(value)) {
                    value = AnnotationSupport.findAnnotation(scope, Baseline.class).get().value().realize();
                    store.put("baseline", value);
                }
                return value;
            }
            catch (Exception e) {
                throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()), e);
            }
        }

        /**
         * Searches for a an annotation of the {@link Baseline} type from local scope to upper, starting from the local parameter
         * of the given parameter context and, if not found, proceeds searching on upper scopes: the declaring method,
         * the decared class and container classes.
         * 
         * If found a {@link Script} is returned representing the state of the {@link Baseline}. If the {@link Baseline} annotation
         * is applied to a shared context (declaring method or class) then it's stored for future use by objects resolving
         * the {@link Baseline} from the same scope.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the scope created by reading parameters from the {@link Baseline} annotation, never {@code null}
         * 
         * @throws ParameterResolutionException if no {@link Baseline} can be found or an issue occurred while creating the
         * script instance
         */
        protected Script resolveBaseline(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.isAnnotated(Baseline.class)) {
                // resolve at the parameter level, no storage is used
                try {
                    return parameterContext.findAnnotation(Baseline.class).get().value().realize();
                }
                catch (Exception e) {
                    throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()), e);
                }
            }
            else if (AnnotationSupport.isAnnotated(parameterContext.getDeclaringExecutable(), Baseline.class)) {
                // resolve at the method level, and store the reference for other consumers
                // when storing use the name of the declaring method as the namespace, from proper isolation
                return resolveSharedBaseline(parameterContext, extensionContext, parameterContext.getDeclaringExecutable(), parameterContext.getDeclaringExecutable().toGenericString());
            }
            else if (AnnotationSupport.isAnnotated(parameterContext.getDeclaringExecutable().getDeclaringClass(), Baseline.class)) {
                // resolve at the class or interface level, and store the reference for other consumers
                // when storing use the name of the declaring class as the namespace, from proper isolation
                return resolveSharedBaseline(parameterContext, extensionContext, parameterContext.getDeclaringExecutable().getDeclaringClass(), parameterContext.getDeclaringExecutable().getDeclaringClass().toGenericString());
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because no %s annotation is present", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), Baseline.class.getName()));
        }

        /**
         * Searches for a an annotation of the {@link CommandSelector} type from local scope to upper, starting from the local parameter
         * of the given parameter context and, if not found, proceeds searching on the declaring method.
         * 
         * If found a {@link Commands} is returned representing the requested command.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the requested command by reading parameters from the {@link CommandSelector} annotation, never {@code null}
         * 
         * @throws ParameterResolutionException if no {@link CommandSelector} can be found
         */
        protected Commands resolveCommandSelector(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.isAnnotated(CommandSelector.class)) {
                return parameterContext.findAnnotation(CommandSelector.class).get().value();
            }
            else if (AnnotationSupport.isAnnotated(parameterContext.getDeclaringExecutable(), CommandSelector.class)) {
                return AnnotationSupport.findAnnotation(parameterContext.getDeclaringExecutable(), CommandSelector.class).get().value();
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because it does not have a % annotation", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), CommandSelector.class.getName()));
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Script} or {@link Workbench}
     * when at least one annotation of type {@link Baseline} is found.
     * When the annotation is applied to parameters it overrides other annotations of the same type
     * at the method level, which in turn overrides those at the class or interface level.
     */
    public static class BaselineParameterResolver extends AbstractParameterResolver {
        /**
         * Default constructor.
         */
        public BaselineParameterResolver() {
            super(Script.class);
        }

        /**
         * Resolves the required parameter according to the definition of the {@link Baseline} annotation.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link Script} object
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public Script resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(Script.class)) {
                return resolveBaseline(parameterContext, extensionContext);
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Command} and {@link CommandProxy} and returns
     * direct implementations of the {@link CommandProxy} interface, used for standalone command invocations.
     */
    public static class StandaloneCommandParameterResolver extends AbstractParameterResolver {
        /**
         * Default constructor.
         */
        public StandaloneCommandParameterResolver() {
            super(CommandProxy.class);
        }

        /**
         * Resolves the required parameter.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link CommandProxy} object, never {@code null}
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public CommandProxy resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(CommandProxy.class)) {
                // first off let's find out which command has to be created
                Commands commandName = resolveCommandSelector(parameterContext, extensionContext);

                // now resolve the Baseline to get a valid Script
                Script script = null;
                try {
                    script = resolveBaseline(parameterContext, extensionContext);
                }
                catch (ParameterResolutionException pre) {
                    throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because no annotation %s could be found or resolved to get the Git Script baseline", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), Baseline.class.getName()), pre);
                }

                try {
                    switch (commandName) {
                        case CLEAN:   return new StandaloneCommandProxy(new Clean(new State(new Configuration()), Git.open(script.getWorkingDirectory())));
                        case INFER:   return new StandaloneCommandProxy(new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory())));
                        case MAKE:    return new StandaloneCommandProxy(new Make(new State(new Configuration()), Git.open(script.getWorkingDirectory())));
                        case MARK:    return new StandaloneCommandProxy(new Mark(new State(new Configuration()), Git.open(script.getWorkingDirectory())));
                        case PUBLISH: return new StandaloneCommandProxy(new Publish(new State(new Configuration()), Git.open(script.getWorkingDirectory())));
                        default:      throw new ParameterResolutionException(String.format("Unable to instantiate command % because it's unknown", commandName));
                    }
                }
                catch (Exception e)
                {
                    throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because the command could not be instantiated", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()));
                }

            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Command} and {@link CommandProxy} and returns
     * implementations that run commands through the {@link Nyx} class using one of its business methods.
     */
    public static class NyxCommandParameterResolver extends AbstractParameterResolver {
        /**
         * Default constructor.
         */
        public NyxCommandParameterResolver() {
            super(CommandProxy.class);
        }

        /**
         * Resolves the required parameter.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link CommandProxy} object, never {@code null}
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public CommandProxy resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(CommandProxy.class)) {
                // first off let's find out which command has to be created
                Commands commandName = resolveCommandSelector(parameterContext, extensionContext);

                // now resolve the Baseline to get a valid Script
                Script script = null;
                try {
                    script = resolveBaseline(parameterContext, extensionContext);
                }
                catch (ParameterResolutionException pre) {
                    throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because no annotation %s could be found or resolved to get the Git Script baseline", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), Baseline.class.getName()), pre);
                }

                try {
                    return new NyxCommandProxy(new Nyx(script.getWorkingDirectory()), commandName);
                }
                catch (Exception e)
                {
                    throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because the command could not be instantiated", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()));
                }

            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }
}
