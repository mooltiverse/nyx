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

import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;

/**
 * This annotation can be used to define the type of command to inject when injecting a {@link Command} or
 * {@link CommandProxy} into a test method as a method parameter.
 * By mean of this annotation the command factory knows which command needs to be proxied and is able
 * to build the proxy for that specific command.
 * This annotation can be applied on the method or the {@link Command} or {@link CommandProxy} parameter to be injected. When
 * applied to the method it is shared by other parameters that may be injected on the same test method,
 * while when applied on the parameter its scope is limited to the parameter itself.
 * <br>
 * This annotation has to be used in conjunction with {@link Baseline}.
 * <br>
 * In order for elements using this annotation to be resolved you need to use the
 * {@link CommandInvocationContextProvider} extension (<code>@ExtendWith(CommandInvocationContextProvider.class)</code>)
 * on the test method or its declaring class.
 * 
 * @see CommandInvocationContextProvider
 * @see CommandProxy
 * @see Baseline
 */
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD,ElementType.PARAMETER})
public @interface CommandSelector {
    /**
     * The identifier of the command.
     * 
     * @return the identifier of the command.
     */
    public Commands value();
}
