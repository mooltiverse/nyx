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

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;
import com.mooltiverse.oss.nyx.git.Workbench;

/**
 * This annotation can be used to inject a Git predefined scenario into tests. This scenario comes in the form of a
 * {@link Script}, {@link Workbench} object injected or the repository used to instantiate an injected {@link Command}.
 * <br>
 * The value for this annotation is the enum value representing the {@link Scenario} to be realized for the annotated element.
 * <br>
 * This annotation is meaningful when resolving items of type {@link Script}, {@link Workbench} or {@link Command} and can be
 * applied on test method parameters, methods or classes. When applied to parameters it overrides other annotations of
 * the same type at the method level, which in turn overrides those at the class or interface level.
 * <br>
 * The instance of the resolved value is scoped to where the annotation is applied so if, for example, is applied at
 * the method level, a parameter of the {@link Script}, {@link Workbench} or {@link Command} type will receive the
 * same script instance unless it defines the annotation again (with the same or another value).
 * <br>
 * In order for elements using this annotation to be resolved you need to use the {@link CommandInvocationContextProvider}.
 * 
 * @see CommandInvocationContextProvider
 */
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD,ElementType.PARAMETER,ElementType.TYPE})
public @interface Baseline {
    /**
     * The scenario to be realized for the injected element.
     * 
     * @return the scenario to be realized for the injected element.
     */
    public Scenario value();
}
