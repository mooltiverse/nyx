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
 * This annotation can be used to define a well known Git baseline to be realized in a Git repository when
 * a test runs. Available baselines are defined as {@link Scenario}s, which indeed is the value that must be
 * declared by this annotation. Baselines are realized in anew temporary directory each time.
 * <br>
 * The baseline being realized may be directly represented by a parameter that is injected into a test method
 * (when the injected parameter is of type {@link Script}, {@link Workbench}) or osed by other objects
 * (i.e. of type {@link Command}), which need a local Git repository to be realized before they are created.
 * <br>
 * This annotation can be applied directly on test method parameters, test methods or their declaring classes
 * and where it is defined also defines its scope. When applied on a method parameter it's scope is not shared
 * so, for example, if you define two {@link Script} parameters and the annotation is on parameters, their
 * scenario is not shared and you must define the annotation on each parameter. If it's defined on the test method
 * all parameters that need a baseline will use the same baseline unless they override at the parameter level.
 * If you apply the annotation at the class level then it will be shared among all methods within the class
 * and their parameters, unless overridden at the method or parameter scope.
 * <br>
 * In order for elements using this annotation to be resolved you need to use the
 * {@link CommandInvocationContextProvider} extension (<code>@ExtendWith(CommandInvocationContextProvider.class)</code>)
 * on the test method or its declaring class.
 * 
 * @see Command
 * @see CommandInvocationContextProvider
 * @see Script
 * @see Workbench
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
