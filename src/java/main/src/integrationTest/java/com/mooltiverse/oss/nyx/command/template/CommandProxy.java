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

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.command.Command;

/**
 * This interface models a proxy used to run a {@code Command} in some context.
 * <br>
 * This interface can be implemented by different contexts in order to run a certain command
 * in a context specific way.
 * <br>
 * For example this package provides two different contexts: one to run commands as standalone
 * objects and another to run the command through the {@link Nyx} class business methods.
 */
public interface CommandProxy extends Command {
    /**
     * Returns the name of the context this proxy run in.
     * 
     * @return the name of the context this proxy run in.
     */
    public String getContextName();
}
