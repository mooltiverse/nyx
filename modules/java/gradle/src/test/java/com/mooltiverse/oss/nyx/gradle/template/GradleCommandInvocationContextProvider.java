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
package com.mooltiverse.oss.nyx.gradle.template;

import java.lang.reflect.Executable;
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

import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.testfixtures.ProjectBuilder;

import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.git.Script;
import com.mooltiverse.oss.nyx.git.Workbench;
import com.mooltiverse.oss.nyx.gradle.CleanTask;
import com.mooltiverse.oss.nyx.gradle.InferTask;
import com.mooltiverse.oss.nyx.gradle.MakeTask;
import com.mooltiverse.oss.nyx.gradle.MarkTask;
import com.mooltiverse.oss.nyx.gradle.PublishTask;
import com.mooltiverse.oss.nyx.gradle.CoreTask;
import com.mooltiverse.oss.nyx.gradle.NyxPlugin;

/**
 * This is an invocation context provider to be used with test templates ({@link TestTemplate}) to produce invocation
 * contexts for such templates and resolve additional annotations and parameters to be used as fixtures.
 * See the <a href="https://junit.org/junit5/docs/current/user-guide/#extensions">Gradle Extension Model</a> documentation for more.
 * <br>
 * With more details, for every template this provider produces one context to test commands invoked as Gradle tasks.
 * <br>
 * Moreover, this extension can inject common entities as method parameters so that you don't need to repeat the same
 * initialization code for all tests. The type of parameters that can be injected are {@link CommandProxy} (or {@link Command}),
 * {@link Script}, {@link Workbench}, {@link Project} and {@link Task}.
 * <br>
 * In order to use this in templates you need to register this provider as a test extension declaratively in a test method,
 * like:<br>
 * <pre>
 *   @TestTemplate
 *   @ExtendWith(GradleCommandInvocationContextProvider.class)
 *   public void myTest(...) {
 *      ...
 *   }
 * </pre>
 * <br>
 * or at the class level, like:<br>
 * <pre>
 *   @ExtendWith(GradleCommandInvocationContextProvider.class)
 *   public class MyTestClass {
 *      ...
 *   }
 * </pre>
 * <br>
 * This way you can have all of the fixture parameters properly injected, if you declare them.
 * Remember that the test method must use the {@link TestTemplate} annotation.
 * <br>
 * Parameters that can be resolved by this extension are:<br>
 * - {@link Command} or {@link CommandProxy}: while you can use it just like other commands injected (see the 
 *   {@link CommandInvocationContextProvider} ducumentation for more) the difference here is that the injected
 *   objects are backed by a Gradle task so when you run them, your are actually triggering a Gradle task
 *   (which in turn runs the backing command). This is useful to test a command using the Gradle plugin
 * - {@link Script} and {@link Workbench}: see the {@link CommandInvocationContextProvider} ducumentation
 * - Gradle {@link Project}: whenever you declare a parameter of type {@link Project} in a test parameter
 *   a new Gradle project is injected. The injected project already has the {@link NyxPlugin} applied
 *   so tasks and extensions are already available. The {@link Baseline} annotation must also be
 *   applied on the parameter or the method or the declaring class to let the provider know
 *   the desired state of the Git repository that the command needs to run in.
 * - Gradle {@link Task}: when you declare a {@link Task} parameter it is injected with a task object
 *   running the task defined by the {@link CommandSelector} annotation, which must be present.
 *   Moreover, like for {@link Project} parameters, the {@link Baseline} annotation must be present.
 *   Objects of this type injected by this provider are almost equal to those injected as {@link Command} or
 *   {@link CommandProxy}. It's up to you to use what you need to run your test.
 * <br>
 * All the parameter types injected by the {@link CommandInvocationContextProvider} are also available through
 * this extension, but they may be backed by different implementations (like for the {@link CommandProxy}).
 * <br>
 * Example:<br>
 * <pre>
 *   @TestTemplate
 *   @ExtendWith(CommandInvocationContextProvider.class)
 *   @Baseline(Scenario.INITIAL_COMMIT)
 *   public void myTest(Project project, @CommandSelector(Commands.PUBLISH) CommandProxy command, Task task, Script script) {
 *      ...
 *   }
 * </pre>
 * will have the {@code command} and the {@code task} running the {@code publish} command in a
 * directory with a Git repository initialized with just an initial commit.
 * The {@code script} parameter will be using the same Git directory and the {@code project} will also
 * have its project directory running in the same directory of the {@code script}.
 * 
 * @see TestTemplate
 */
public class GradleCommandInvocationContextProvider extends CommandInvocationContextProvider {
    /**
     * Default constructor
     */
    public GradleCommandInvocationContextProvider() {
        super();
    }

    /**
     * Returns a stream with one invocation context for invoking the command as a Gradle task.
     * 
     * @return a stream with one invocation context for invoking the command as a Gradle task.
     */
    @Override
    public Stream<TestTemplateInvocationContext> provideTestTemplateInvocationContexts(ExtensionContext context) {
        return Stream.<TestTemplateInvocationContext>of(new GradleTaskInvocationContext(context.getDisplayName()));
    }

    /**
     * The context used to invoke commands using the as Gradle tasks. 
     */
    private static class GradleTaskInvocationContext implements TestTemplateInvocationContext {
        /**
         * The display name passed in the constructor and also returned by {@link #getDisplayName(int)}.
         */
        private String displayName = null;

        /**
         * Standard constructor.
         * 
         * @param displayName the display name to be returned when {@link #getDisplayName(int)} is invoked.
         */
        GradleTaskInvocationContext(String displayName) {
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
            return Objects.isNull(displayName) ? "[context: gradle    ] ".concat(Integer.toString(invocationIndex)) : "[context: gradle    ] ".concat(displayName);
        }

        /**
         * Returns the list of additional extensions (parameter resolvers) for this invocation.
         * 
         * @return the list of additional extensions (parameter resolvers) for this invocation.
         */
        @Override
        public List<Extension> getAdditionalExtensions() {
            return List.<Extension>of(new GradleProjectParameterResolver(), new GradleTaskParameterResolver(), new GradleCommandParameterResolver(), new BaselineParameterResolver());
        }
    }

    /**
     * The common superclass for Gradle parameter resolvers implemented by this provider.
     */
    public static abstract class GradleAbstractParameterResolver extends AbstractParameterResolver {
        /**
         * Constructor.
         * 
         * @param parameterType the type of the parameter resolved by the resolver. If more types are resolved but they
         * have a common superclass, pass the superclass here.
         */
        protected GradleAbstractParameterResolver(Class<?> parameterType) {
            super(parameterType);
        }

        /**
         * Resolves the Gradle {@link Project} parameter relatively to the given (test) method scope by returning a previously stored instance of the
         * project relatively to the given method. If no previous instance is created, a new one is instantiated and stored for further use
         * by other parameters on the same method.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * @param scope the method scope the parameter is resolved against
         * 
         * @return the resolved {@link Project} object, never {@code null}
         * 
         * @throws ParameterResolutionException if the parameter cannot be resolved
         */
        protected Project resolveSharedProject(ParameterContext parameterContext, ExtensionContext extensionContext, Executable scope)
            throws ParameterResolutionException {
            try {
                ExtensionContext.Store store = extensionContext.getStore(ExtensionContext.Namespace.create(scope.toGenericString()));
                Project project = store.get("project", Project.class);
                if (Objects.isNull(project)) {
                    Script script = resolveBaseline(parameterContext, extensionContext);
                    project = ProjectBuilder.builder().withProjectDir(script.getWorkingDirectory()).build();
                    project.getPluginManager().apply(NyxPlugin.ID);
                    store.put("project", project);
                }
                return project;
            }
            catch (Exception e) {
                throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()), e);
            }
        }

        /**
         * Resolves the Gradle {@link Task} parameter relatively to the given (test) method scope by returning a previously stored instance of the
         * project relatively to the given method. If no previous instance is created, a new one is instantiated and stored for further use
         * by other parameters on the same method.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * @param scope the method scope the parameter is resolved against
         * 
         * @return the resolved {@link Task} object, never {@code null}
         * 
         * @throws ParameterResolutionException if the parameter cannot be resolved
         */
        protected Task resolveSharedTask(ParameterContext parameterContext, ExtensionContext extensionContext, Executable scope)
            throws ParameterResolutionException {
            try {
                ExtensionContext.Store store = extensionContext.getStore(ExtensionContext.Namespace.create(scope.toGenericString()));
                Task task = store.get("task", Task.class);
                if (Objects.isNull(task)) {
                    Project project = resolveSharedProject(parameterContext, extensionContext, scope);
                    Commands commandName = resolveCommandSelector(parameterContext, extensionContext);
                    String taskName = null;
                    switch (commandName) {
                        case CLEAN:   taskName = CleanTask.NAME; break;
                        case INFER:   taskName = InferTask.NAME; break;
                        case MAKE:    taskName = MakeTask.NAME; break;
                        case MARK:    taskName = MarkTask.NAME; break;
                        case PUBLISH: taskName = PublishTask.NAME; break;
                        default:      throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because the command % is unknown", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), commandName.toString()));
                    }
                    task = project.getTasks().getByName(taskName);
                    store.put("task", task);
                }
                return task;
            }
            catch (Exception e) {
                throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName()), e);
            }
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Project}.
     */
    private static class GradleProjectParameterResolver extends GradleAbstractParameterResolver {
        /**
         * Default constructor.
         */
        GradleProjectParameterResolver() {
            super(Project.class);
        }

        /**
         * Resolves the required parameter.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link Project} object, never {@code null}
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public Project resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(Project.class)) {
                return resolveSharedProject(parameterContext, extensionContext, parameterContext.getDeclaringExecutable());
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Task} and returns implementations that run commands
     * as Gradle tasks.
     */
    private static class GradleTaskParameterResolver extends GradleAbstractParameterResolver {
        /**
         * Default constructor.
         */
        GradleTaskParameterResolver() {
            super(Task.class);
        }

        /**
         * Resolves the required parameter.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link Task} object, never {@code null}
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public Task resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(Task.class)) {
                return resolveSharedTask(parameterContext, extensionContext, parameterContext.getDeclaringExecutable());
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }

    /**
     * This parameter resolver can resolve parameters of type {@link Command} and returns implementations that run commands
     * as Gradle tasks.
     */
    private static class GradleCommandParameterResolver extends GradleAbstractParameterResolver {
        /**
         * Default constructor.
         */
        GradleCommandParameterResolver() {
            super(CommandProxy.class);
        }

        /**
         * Resolves the required parameter.
         * 
         * @param parameterContext the parameter context
         * @param extensionContext the extension context
         * 
         * @return the resolved {@link Command} object, never {@code null}
         * 
         * @see ParameterResolver#resolveParameter(ParameterContext, ExtensionContext)
         */
        @Override
        public CommandProxy resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            if (parameterContext.getParameter().getType().isAssignableFrom(CommandProxy.class)) {
                return new GradleTaskCommand(CoreTask.class.cast(resolveSharedTask(parameterContext, extensionContext, parameterContext.getDeclaringExecutable())));
            }
            else throw new ParameterResolutionException(String.format("Cannot resolve parameter %s in %s because its type %s is not supported by this resolver", parameterContext.getParameter().getName(), parameterContext.getParameter().getDeclaringExecutable().getName(), parameterContext.getParameter().getType().getName()));
        }
    }
}
