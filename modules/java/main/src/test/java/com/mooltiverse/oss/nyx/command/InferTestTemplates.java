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
package com.mooltiverse.oss.nyx.command;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.configuration.presets.Extended;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

@DisplayName("Infer")
public class InferTestTemplates {
    /**
     * A {@link MethodSource} method that returns valid structured data to test parseable (into version range regexps) branch names
     * and the version range check regular expressions to be generated from them.
     * Each returned argument has the fields:<br>
     * - branch name: the name of a branch<br>
     * - regex: the regular expression that is expected to be generated from that branch name<br>
     *
     * @return a stream of arguments
     */
    static Stream<Arguments> wellKnownParseableBranchNames() {
        return Stream.of(
            arguments("1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("v1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relv1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/v1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-v1", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.x.x", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2.x", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2.3", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),

            arguments("1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("v1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("v1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relv1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relv1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/v1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/v1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-v1-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.x.x-abc.123+def.456", "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2.x-abc.123+def.456", "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-v1.2.3-abc.123+def.456", "^1\\.2\\.3(?:(?:-|\\+).*)?$"),

            arguments("x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("vx", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relx", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relvx", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/vx", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-vx", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.x.x", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2.x", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2.3", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),

            arguments("x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("x.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("vx-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("vx.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relx-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relx.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("relvx-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("relvx.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/x.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("rel/vx-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("rel/vx.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-x.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"),
            arguments("release-vx-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.x.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2.x-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"),
            arguments("release-vx.2.3-abc.123+def.456", "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$")
        );
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test unparseable (into version range regexps) branch names.
     * Each returned argument has the fields:<br>
     * - branch name: the name of a branch<br>
     *
     * @return a stream of arguments
     */
    static Stream<Arguments> wellKnownUnparseableBranchNames() {
        return Stream.of(
            // these values are unparseable because of invalid suffixes not separated by - or +
            arguments("1abc"),
            arguments("1.xabc"),
            arguments("1.x.xabc"),
            arguments("1.2abc"),
            arguments("1.2.xabc"),
            arguments("1.2.3abc"),
            arguments("xabc"),
            arguments("x.xabc"),
            arguments("x.x.xabc"),
            arguments("x.2abc"),
            arguments("x.2.xabc"),
            arguments("x.2.3abc"),
            arguments("vabc"),
            arguments("v1abc"),
            arguments("v1.xabc"),
            arguments("v1.x.xabc"),
            arguments("v1.2abc"),
            arguments("v1.2.3abc"),
            arguments("vxabc"),
            arguments("vx.xabc"),
            arguments("vx.x.xabc"),
            arguments("vx.2abc"),
            arguments("vx.2.3abc"),
            arguments("rel1abc"),
            arguments("rel1.xabc"),
            arguments("rel1.x.xabc"),
            arguments("rel1.2abc"),
            arguments("rel1.2.xabc"),
            arguments("rel1.2.3abc"),
            arguments("relxabc"),
            arguments("relx.xabc"),
            arguments("relx.x.xabc"),
            arguments("relx.2abc"),
            arguments("relx.2.xabc"),
            arguments("relx.2.3abc"),
            arguments("relvabc"),
            arguments("relv1abc"),
            arguments("relv1.xabc"),
            arguments("relv1.x.xabc"),
            arguments("relv1.2abc"),
            arguments("relv1.2.3abc"),
            arguments("relvabc"),
            arguments("relvxabc"),
            arguments("relvx.xabc"),
            arguments("relvx.x.xabc"),
            arguments("relvx.2abc"),
            arguments("relvx.2.3abc"),
            arguments("rel/1abc"),
            arguments("rel/1.xabc"),
            arguments("rel/1.x.xabc"),
            arguments("rel/1.2abc"),
            arguments("rel/1.2.xabc"),
            arguments("rel/1.2.3abc"),
            arguments("rel/xabc"),
            arguments("rel/x.xabc"),
            arguments("rel/x.x.xabc"),
            arguments("rel/x.2abc"),
            arguments("rel/x.2.xabc"),
            arguments("rel/x.2.3abc"),
            arguments("rel/vabc"),
            arguments("rel/v1abc"),
            arguments("rel/v1.xabc"),
            arguments("rel/v1.x.xabc"),
            arguments("rel/v1.2abc"),
            arguments("rel/v1.2.3abc"),
            arguments("rel/vabc"),
            arguments("rel/vxabc"),
            arguments("rel/vx.xabc"),
            arguments("rel/vx.x.xabc"),
            arguments("rel/vx.2abc"),
            arguments("rel/vx.2.3abc"),
            arguments("release-1abc"),
            arguments("release-1.xabc"),
            arguments("release-1.x.xabc"),
            arguments("release-1.2abc"),
            arguments("release-1.2.xabc"),
            arguments("release-1.2.3abc"),
            arguments("release-xabc"),
            arguments("release-x.xabc"),
            arguments("release-x.x.xabc"),
            arguments("release-x.2abc"),
            arguments("release-x.2.xabc"),
            arguments("release-x.2.3abc"),
            arguments("release-vabc"),
            arguments("release-v1abc"),
            arguments("release-v1.xabc"),
            arguments("release-v1.x.xabc"),
            arguments("release-v1.2abc"),
            arguments("release-v1.2.3abc"),
            arguments("release-vabc"),
            arguments("release-vxabc"),
            arguments("release-vx.xabc"),
            arguments("release-vx.x.xabc"),
            arguments("release-vx.2abc"),
            arguments("release-vx.2.3abc")
        );
    }
    
    @Nested
    @DisplayName("Infer constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Infer()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Infer state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Infer.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Infer isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @TestTemplate
        @DisplayName("Infer.isUpToDate()")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            assertTrue(command.isUpToDate());

            // and running again with no changes must still be up to date
            command.run();
            assertTrue(command.isUpToDate());
        }

        /**
         * Check that the isUpToDate() always returns {@code false} when the repository is dirty.
         */
        @TestTemplate
        @DisplayName("Infer.isUpToDate() == false in dirty repository")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateInDirtyRepositoryTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            assertTrue(command.isUpToDate());

            // but if we add uncommitted files it must return false
            script.addRandomTextWorkbenchFiles(1);
            assertFalse(command.isUpToDate());
            command.run();
            assertFalse(command.isUpToDate());

            // still false even after staging
            script.stage();
            assertFalse(command.isUpToDate());
            command.run();
            assertFalse(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Infer match release type")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ReleaseTypeMatchTests {
        @TestTemplate
        @DisplayName("Infer.run() throws exception when the list of enabled release types contains a non existing release type")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void matchReleaseTypeWithNonExistingReleaseTypeThrowsExceptionTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("nonexisting", "matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED");         // use this value to see if the release type has been matched
                            setMatchBranches("^(master|main)$");    // match main and master
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(IllegalPropertyException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() matches the right release type based on the branch name")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void matchReleaseTypeBasedOnBranchNameTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("unmatched", "matched", "fallback"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "unmatched", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED");       // use this value to see if the release type has been matched
                            setMatchBranches("^nonexistendbranch$");// match only a branch name that doesn't exist
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }},
                        "matched", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED");         // use this value to see if the release type has been matched
                            setMatchBranches("^(master|main)$");    // match main and master
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }},
                        "fallback", new ReleaseType() {
                        {
                            setGitCommitMessage("FALLBACK");        // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the master branch must match the first release type
            script.checkout("master");
            command.run();
            assertEquals("MATCHED", command.state().getReleaseType().getGitCommitMessage());
            assertEquals("^(master|main)$", command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());

            // any other branch must match the fallback release type
            script.checkout("integration");
            command.run();
            assertEquals("FALLBACK", command.state().getReleaseType().getGitCommitMessage());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
        }

        @TestTemplate
        @DisplayName("Infer.run() matches the right release type based on environment variables")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void matchReleaseTypeBasedOnEnvironmentVariablesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            // since we can't set environment variables here we just use the PATH variable, which is always present
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("unmatched", "matchedpath", "fallback"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "unmatched", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED");       // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH","^nonexistingvalue$")); // require the PATH variable to be present, with a non existing value
                            setMatchWorkspaceStatus(null);
                        }},
                        "matchedpath", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED PATH");    // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH",".*")); // require the PATH variable to be present, with any value
                            setMatchWorkspaceStatus(null);
                        }},
                        "fallback", new ReleaseType() {
                        {
                            setGitCommitMessage("FALLBACK");        // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the PATH variable must be matched (with any value)
            command.run();
            assertEquals("MATCHED PATH", command.state().getReleaseType().getGitCommitMessage());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNotNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
        }

        @TestTemplate
        @DisplayName("Infer.run() matches the right release type based on the workspace status")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void matchReleaseTypeBasedOnWorkspaceStatusTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("unmatched", "matchedclean", "matcheddirty", "fallback"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "unmatched", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED");       // use this value to see if the release type has been matched
                            setMatchBranches("^nonexistendbranch$");// match only a branch name that doesn't exist
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }},
                        "matchedclean", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED CLEAN");   // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);  // the workspace must be clean to match
                        }},
                        "matcheddirty", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED DIRTY");   // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(WorkspaceStatus.DIRTY);  // the workspace must be dirty to match
                        }},
                        "fallback", new ReleaseType() {
                        {
                            setGitCommitMessage("FALLBACK");        // use this value to see if the release type has been matched
                            setMatchBranches(null);                 // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the CLEAN release type must be matched
            script.checkout("master");
            command.run();
            assertEquals("MATCHED CLEAN", command.state().getReleaseType().getGitCommitMessage());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(WorkspaceStatus.CLEAN, command.state().getReleaseType().getMatchWorkspaceStatus());

            // make uncommitted changes
            script.updateAllWorkbenchFiles();

            // the DIRTY release type must be matched
            command.run();
            assertEquals("MATCHED DIRTY", command.state().getReleaseType().getGitCommitMessage());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(WorkspaceStatus.DIRTY, command.state().getReleaseType().getMatchWorkspaceStatus());
        }

        @TestTemplate
        @DisplayName("Infer.run() matches the right release type based on branch name, environment variables and workspace status")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void matchReleaseTypeBasedOnBranchNameAndEnvironmentVariablesAndWorkspaceStatusTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            // since we can't set environment variables here we just use the PATH variable, which is always present
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("unmatched", "unmatchedbybranch", "unmatchedbyenvironmentvariables", "unmatchedbyworkspacestatus", "matched", "fallback"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "unmatched", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED");                   // use this value to see if the release type has been matched
                            setMatchBranches("^nonexistendbranch$");            // match only a branch name that doesn't exist
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH","^nonexistingvalue$")); // require the PATH variable to be present, with a non existing value
                            setMatchWorkspaceStatus(null);
                        }},
                        "unmatchedbybranch", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED BY BRANCH");         // use this value to see if the release type has been matched
                            setMatchBranches("^nonexistendbranch$");            // match only a branch name that doesn't exist
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH",".*")); // require the PATH variable to be present, with any value
                            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);     // the workspace must be clean to match
                        }},
                        "unmatchedbyenvironmentvariables", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED BY ENVIRONMENT VARIABLES");         // use this value to see if the release type has been matched
                            setMatchBranches("^(master|main)$");                // match main and master
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH","^nonexistingvalue$")); // require the PATH variable to be present, with a non existing value
                            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);     // the workspace must be clean to match
                        }},
                        "unmatchedbyworkspacestatus", new ReleaseType() {
                        {
                            setGitCommitMessage("UNMATCHED BY WORKSPACE STATUS");         // use this value to see if the release type has been matched
                            setMatchBranches("^(master|main)$");                // match main and master
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH",".*")); // require the PATH variable to be present, with any value
                            setMatchWorkspaceStatus(WorkspaceStatus.DIRTY);     // the workspace must be dirty to match
                        }},
                        "matched", new ReleaseType() {
                        {
                            setGitCommitMessage("MATCHED");                     // use this value to see if the release type has been matched
                            setMatchBranches("^(master|main)$");                // match main and master
                            setMatchEnvironmentVariables(Map.<String,String>of("PATH",".*")); // require the PATH variable to be present, with any value
                            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);     // the workspace must be clean to match
                        }},
                        "fallback", new ReleaseType() {
                        {
                            setGitCommitMessage("FALLBACK");                    // use this value to see if the release type has been matched
                            setMatchBranches(null);                             // match any branch name
                            setMatchEnvironmentVariables(null);
                            setMatchWorkspaceStatus(null);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.checkout("master");
            // the PATH variable must be matched (with any value)
            command.run();
            assertEquals("MATCHED", command.state().getReleaseType().getGitCommitMessage());
            assertEquals("^(master|main)$", command.state().getReleaseType().getMatchBranches());
            assertNotNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(WorkspaceStatus.CLEAN, command.state().getReleaseType().getMatchWorkspaceStatus());
        }
    }
    @Nested
    @DisplayName("Infer version with extra identifiers")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ExtraIdentifiersTests {
        @TestTemplate
        @DisplayName("Infer.run() throws exception when the value of a pre-release identifier is not integer")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraNonIntegerPrereleaseIdentifierThrowsExceptionTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("nonint", "abc", Identifier.Position.PRE_RELEASE)));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(IllegalPropertyException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a pre-release identifier")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraIntegerPrereleaseIdentifierTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("identifier1", "123", Identifier.Position.PRE_RELEASE)));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("0.0.5-identifier1.123", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a pre-release identifier over existing ones")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraIntegerPrereleaseIdentifierOverExistingOnesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("identifier1", "123", Identifier.Position.PRE_RELEASE)));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.andCommit("Untagged commit #1");
            script.andTag("0.1.0-tag1.identifier1.999", null);
            script.andCommit("Untagged commit #2");

            command.run();

            assertEquals("0.1.1-tag1.identifier1.123", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a build identifier")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraBuildIdentifierTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("identifier1", "abc", Identifier.Position.BUILD)));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("0.0.5+identifier1.abc", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a build identifier over existing ones")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraBuildIdentifierOverExistingOnesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("identifier1", "abc", Identifier.Position.BUILD)));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.andCommit("Untagged commit #1");
            script.andTag("0.1.0+tag1.identifier1.999", null);
            script.andCommit("Untagged commit #2");

            command.run();

            assertEquals("0.1.1+tag1.identifier1.abc", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() with multiple identifiers")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraMultipleIdentifiersTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(
                                new Identifier("p1", "123", Identifier.Position.PRE_RELEASE),
                                new Identifier("p2", null, Identifier.Position.PRE_RELEASE),
                                new Identifier("p3", "456", Identifier.Position.PRE_RELEASE),
                                new Identifier("b1", "abc", Identifier.Position.BUILD),
                                new Identifier("b2", null, null), // BUILD is the default position
                                new Identifier("b3", "def", Identifier.Position.BUILD)
                            ));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("0.0.5-p1.123.p2.p3.456+b1.abc.b2.b3.def", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() with multiple identifiers over existing ones")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void extraMultipledentifiersOverExistingOnesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(
                                new Identifier("p1", "123", Identifier.Position.PRE_RELEASE),
                                new Identifier("p2", null, Identifier.Position.PRE_RELEASE),
                                new Identifier("p3", "456", Identifier.Position.PRE_RELEASE),
                                new Identifier("b1", "abc", Identifier.Position.BUILD),
                                new Identifier("b2", null, null), // BUILD is the default position
                                new Identifier("b3", "def", Identifier.Position.BUILD)
                            ));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.andCommit("Untagged commit #1");
            script.andTag("0.1.0-tag1.p2.999+b3.zzz", null);
            script.andCommit("Untagged commit #2");

            command.run();

            // existing extra identifiers will come first with their values overwritten only if the new identifier has a value,
            // then the new identifiers are appended to the end. If a previous identifier has a value and the new one doesn't,
            // the old value remains instead of being cleared by the new one because there is no means to know if that previous
            // value was the identifier value or a tag itself
            assertEquals("0.1.1-tag1.p2.999.p1.123.p3.456+b3.def.b1.abc.b2", command.state().getVersion());
        }
    }

    @Nested
    @DisplayName("Infer version range check")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class VersionRangeCheckTests {
        @TestTemplate
        @DisplayName("Infer.run() with static matching version range")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void versionRangeCheckWithStaticMatchingExpressionTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setVersionRange("^0\\.0\\.([0-9]*)$");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("0.0.5", command.state().getVersion());
            assertEquals("^0\\.0\\.([0-9]*)$", command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() with static non matching version range")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void versionRangeCheckWithStaticNonMatchingExpressionTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setVersionRange("^1\\.2\\.([0-9]*)$");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the generated version does not comply with the version range so it raises an exception
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(ReleaseException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());

            assertEquals("^1\\.2\\.([0-9]*)$", command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() with static malformed version range")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void versionRangeCheckWithStaticMalformedExpressionTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setVersionRange("^1\\.2\\.((((((([0-9]*)$");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the generated version does not comply with the version range so it raises an exception
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(IllegalPropertyException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());

            assertEquals("^1\\.2\\.((((((([0-9]*)$", command.state().getVersionRange());
        }

        /* We should use a @ParameterizedTest here, using the @MethodSource("InferTestTemplates#wellKnownBranchNames")
           but that would break the @TestTemplate because we can une just one or the other in the same method. So we keep
           the @TestTemplate and loop over the data set within the test method.*/
        @TestTemplate
        @DisplayName("Infer.run() with dynamic version range inferred from parseable branch names")
        @Baseline(Scenario.INITIAL_COMMIT)
        void versionRangeCheckWithDynamicExpressionInferredFromParseableBranchNamesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setVersionRangeFromBranchName(Boolean.TRUE);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            //if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // These tests may fail when running in a standalone context because the run() doee not
                // imply up-to-date checks and the execution may inherit stale State values from previous
                // runs. Keep this in mind if they fail and uncomment this conditional so that
                // tests are executed only in non-standalone contexts.
                for (Arguments args: wellKnownParseableBranchNames().collect(Collectors.toList())) {
                    script.checkout(args.get()[0].toString()); // create the branch
                    script.andCommit("An untagged commit"); // create a commit
                    try {
                        command.run();
                    }
                    catch (Exception e) {} // version range check may fail here but we don't care, we're just testing the regex that was inferred
                    assertEquals(args.get()[1], command.state().getVersionRange(), String.format("Testing with branch '%s', the expected inferred dynamic version range check regular expression was '%s' but the actual result was '%s'", args.get()[0].toString(), args.get()[1].toString(), command.state().getVersionRange()));
                }
            //}                
        }

        /* We should use a @ParameterizedTest here, using the @MethodSource("InferTestTemplates#wellKnownBranchNames")
           but that would break the @TestTemplate because we can une just one or the other in the same method. So we keep
           the @TestTemplate and loop over the data set within the test method.*/
        @TestTemplate
        @DisplayName("Infer.run() with dynamic version range inferred from unparseable branch names")
        @Baseline(Scenario.INITIAL_COMMIT)
        void versionRangeCheckWithDynamicExpressionInferredFromUnparseableBranchNamesTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add some fictional release types
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("matched"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "matched", new ReleaseType() {
                        {
                            setVersionRangeFromBranchName(Boolean.TRUE);
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            //if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // These tests may fail when running in a standalone context because the run() doee not
                // imply up-to-date checks and the execution may inherit stale State values from previous
                // runs. Keep this in mind if they fail and uncomment this conditional so that
                // tests are executed only in non-standalone contexts.
                for (Arguments args: wellKnownUnparseableBranchNames().collect(Collectors.toList())) {
                    script.checkout(args.get()[0].toString()); // create the branch
                    script.andCommit("An untagged commit"); // create a commit
                    assertThrows(Exception.class, () -> command.run()); // this always throws an exception with these branches
                    // the command is not able to parse a valid version range expression from these values so it must yield to null
                    assertNull(command.state().getVersionRange(), String.format("Testing with branch '%s', the expected inferred dynamic version range check regular expression was null but the actual result was '%s'", args.get()[0].toString(), command.state().getVersionRange()));
                }
            //}                
        }
    }

    @Nested
    @DisplayName("Infer run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Infer.run() throws exception with a valid but empty Git repository in working directory")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionOnRunWithValidButEmptyGitRepositoryTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, using defaults > yield to previous=initial version, no new version or release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(Defaults.INITIAL_VERSION.toString(), command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with initial version override > yield to previous=initial version, no new version or release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndInitialVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_INITIAL_VERSION = "12.13.14";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setInitialVersion(CUSTOM_INITIAL_VERSION);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(CUSTOM_INITIAL_VERSION, command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with version override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_VERSION = "1.2.3";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion(CUSTOM_VERSION);
            command.run();

            assertNull(command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(CUSTOM_VERSION, command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with bump=major override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMajorOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMinorOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.2.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpPatchOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with the 'Initial commit' only, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpAlphaOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with one versioned commit, with bump=major override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with one versioned commit, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.2.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with one versioned commit, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with one versioned commit, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with prefixed versioned commit, with release lenient, without prefix > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithReleaseLenientInRepoWithPrefixedVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("2.2.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with prefixed versioned commit, without release lenient, without prefix > yield to no new version or release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientInRepoWithoutPrefixedVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with prefixed versioned commit, without release lenient, with prefix > yield to no new version or release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("release-2.2.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with initial version override > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsWithInitialVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setInitialVersion("12.13.14");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with version override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion("1.2.3");
            command.run();

            assertNull(command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.2.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with bump=major override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpMajorOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpMinorOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpPatchOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.5", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpAlphaOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4-alpha.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with release lenient, without prefix > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("2.2.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, without release lenient, without prefix > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, without release lenient, with prefix > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientAndWithPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("release-2.2.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and a commit with overlapping valid tags > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_WITH_OVERLAPPING_TAGS)
        void runUsingDefaultReleaseTypeInRepoWithOverlappingTagsCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.6", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.6"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.7", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with a commit message convention that accepts all commits as significant > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithAlwaysPositiveCommitConventionInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseScope().getSignificantCommits().get(0).getSHA().equals(script.getCommitIDs().get(0)));
            assertTrue(command.state().getReleaseScope().getSignificantCommits().get(1).getSHA().equals(script.getCommitIDs().get(1)));
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() using default release type on repository with simple linear commit history and non significant commits, with a commit message convention that accepts no commits as significant > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithAlwaysNegativeCommitConventionInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in master branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("master", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in master branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.5", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in main branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMainBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("main");
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("main", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in main branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMainBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("main");
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("main", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in integration branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInIntegrationBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("integration");
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("integration", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6-integration.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in integration branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInIntegrationBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("integration");
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("integration", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-integration.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in development branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInDevelopmentBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("development");
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("development", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.1-development.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in development branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInDevelopmentBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("development");
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("development", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
            assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in alpha branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInAlphaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("alpha");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("alpha", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-alpha.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-alpha.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6-alpha.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in alpha branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInAlphaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("alpha");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("alpha", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-alpha.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-alpha.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-alpha.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in beta branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInBetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("beta");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("beta", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-beta.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-beta.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6-beta.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in beta branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInBetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("beta");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("beta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-beta.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-beta.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-beta.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in gamma branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInGammaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("gamma");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("gamma", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(7, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6-gamma.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in gamma branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInGammaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("gamma");
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("gamma", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(7, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.5", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in v0.x branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV0xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("v0.x");
            final String MATCHING_RELEASE_TYPE_NAME = "maintenance"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("v0.x", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.8", command.state().getVersion());
            assertNotNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in v0.x branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInV0xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("v0.x");
            final String MATCHING_RELEASE_TYPE_NAME = "maintenance"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("v0.x", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.7", command.state().getVersion());
            assertNotNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in v1.x branch > yield to version range check exception")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV1xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("v1.x");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the generated version does not comply with the version range so it raises an exception
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(ReleaseException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in rel/0.x branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel0xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("rel/0.x");
            final String MATCHING_RELEASE_TYPE_NAME = "release"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("rel/0.x", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-rel.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-rel.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-rel.3", command.state().getVersion());
            assertNotNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in rel/0.x branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInRel0xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("rel/0.x");
            final String MATCHING_RELEASE_TYPE_NAME = "release"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("rel/0.x", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-rel.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-rel.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-rel.2", command.state().getVersion());
            assertNotNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in rel/1.x branch > yield to version range check exception")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel1xBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("rel/1.x");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            // the generated version does not comply with the version range so it raises an exception
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertThrows(ReleaseException.class, () -> command.run());
            else assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in feature/SSO branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureSSOBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("feature/SSO");
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("feature/SSO", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-featuresso.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-featuresso.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-featuresso.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in feature/SSO branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureSSOBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("feature/SSO");
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("feature/SSO", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-featuresso.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-featuresso.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-featuresso.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in feature/IN-12345 branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureIN12345BranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("feature/IN-12345");
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("feature/IN-12345", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(7, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-featurein12345.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in feature/IN-12345 branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureIN12345BranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("feature/IN-12345");
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("feature/IN-12345", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(7, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.5", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the hotfix release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in fix-98765 branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFix98765BranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("fix-98765");
            final String MATCHING_RELEASE_TYPE_NAME = "hotfix"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("fix-98765", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.8-fix98765.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.8-fix98765.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.8-fix98765.3", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the hotfix release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in fix-98765 branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFix98765BranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("fix-98765");
            final String MATCHING_RELEASE_TYPE_NAME = "hotfix"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("fix-98765", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.8-fix98765.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.8-fix98765.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.8-fix98765.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in internal branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInInternalBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("internal");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("internal", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            // the version contains the timestamp which is variable so let's test the start string and the overall length
            assertTrue(command.state().getVersion().startsWith("0.0.6-internal.2+timestamp."), String.format("Version '%s' was expected to start with '%s'", command.state().getVersion(), "0.0.6-internal.2+timestamp."));
            assertEquals("0.0.6-internal.2+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in internal branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInInternalBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("internal");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("internal", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in somebranch branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomebranchBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("somebranch");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("somebranch", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            // the version contains the timestamp which is variable so let's test the start string and the overall length
            assertTrue(command.state().getVersion().startsWith("0.0.6-internal.1+timestamp."), String.format("Version '%s' was expected to start with '%s'", command.state().getVersion(), "0.0.6-internal.1+timestamp."));
            assertEquals("0.0.6-internal.1+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in somebranch branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomebranchBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("somebranch");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("somebranch", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(4, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-integration.2", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in someotherbranch branch > yield to new version but no new release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomeotherbranchBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("someotherbranch");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("someotherbranch", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(5, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            // the version contains the timestamp which is variable so let's test the start string and the overall length
            assertTrue(command.state().getVersion().startsWith("0.0.6-internal.1+timestamp."), String.format("Version '%s' was expected to start with '%s'", command.state().getVersion(), "0.0.6-internal.1+timestamp."));
            assertEquals("0.0.6-internal.1+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in someotherbranch branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomeotherbranchBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("someotherbranch");
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("someotherbranch", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-integration.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-integration.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getDescription(), command.state().getReleaseType().getDescription());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().containsAll(command.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().size(); i++) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
            assertEquals(command.state().getConfiguration().getReleaseTypes().getItems().get(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-integration.1", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom release type on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in internal branch > yield to new version and release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingCustomReleaseTypeWithAlwaysPositiveCommitConventionInInternalBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("internal");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseType", new ReleaseType() {
                        {
                            setCollapseVersions(true);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                            setIdentifiers(List.<Identifier>of(new Identifier("customId", "999", Identifier.Position.PRE_RELEASE)));
                            setPublish(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.andAddFiles(); // add some uncommitted changes to be committed by the Mark command
            command.run();

            assertEquals("internal", command.state().getBranch());
            assertEquals("patch", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(1, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals("customId", command.state().getReleaseType().getIdentifiers().get(0).getQualifier());
            assertEquals("999", command.state().getReleaseType().getIdentifiers().get(0).getValue());
            assertEquals(Identifier.Position.PRE_RELEASE, command.state().getReleaseType().getIdentifiers().get(0).getPosition());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getPublish());
            assertNull(command.state().getReleaseType().getVersionRange());
            assertFalse(command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.6-internal.2.customId.999+timestamp.003", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom release type on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in internal branch > yield to no new version or release")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingCustomReleaseTypeWithAlwaysNegativeCommitConventionInInternalBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("internal");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of()))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseType", new ReleaseType() {
                        {
                            setCollapseVersions(true);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                            setIdentifiers(List.<Identifier>of(new Identifier("customId", "999", Identifier.Position.PRE_RELEASE)));
                            setPublish(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            script.andAddFiles(); // add some uncommitted changes to be committed by the Mark command
            command.run();

            assertEquals("internal", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals("customId", command.state().getReleaseType().getIdentifiers().get(0).getQualifier());
            assertEquals("999", command.state().getReleaseType().getIdentifiers().get(0).getValue());
            assertEquals(Identifier.Position.PRE_RELEASE, command.state().getReleaseType().getIdentifiers().get(0).getPosition());
            assertNull(command.state().getReleaseType().getMatchBranches());
            assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
            assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getPublish());
            assertNull(command.state().getReleaseType().getVersionRange());
            assertFalse(command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.6-internal.1+timestamp.003", command.state().getVersion());
            assertNull(command.state().getVersionRange());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in master branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithInferringCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in taggedwithoutbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithInferringCommitConventionInTaggedwithoutbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("taggedwithoutbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("taggedwithoutbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.2.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.2.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.2.3", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.2.3"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.2.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in taggedwithbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithInferringCommitConventionInTaggedwithbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("taggedwithbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("taggedwithbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.3.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.3.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.3.3", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.3.3"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.3.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in untaggedwithoutbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithInferringCommitConventionInUntaggedwithoutbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("untaggedwithoutbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("untaggedwithoutbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in untaggedwithbump branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithInferringCommitConventionInUntaggedwithbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("untaggedwithbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("untaggedwithbump", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in master branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals("^(master|main)$", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in alpha branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInAlphaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("alpha");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("alpha", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-alpha.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-alpha.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-alpha.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in beta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInBetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("beta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("beta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-beta.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-beta.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-beta.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in gamma branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInGammaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("gamma");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("gamma", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in delta branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInDeltaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("delta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("delta", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-delta.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in epsilon branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInEpsilonBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("epsilon");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("epsilon", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("1.0.0-epsilon.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-epsilon.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-epsilon.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in zeta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInZetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("zeta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("zeta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-zeta.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-zeta.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-zeta.4", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in eta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInEtaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("eta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("eta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("1.0.0-eta.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-eta.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-eta.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in theta branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInThetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("theta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("theta", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-theta.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type with extra identifier on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in master branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type with extra identifier on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in taggedwithoutbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInTaggedwithoutbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("taggedwithoutbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("taggedwithoutbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.2.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.2.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.2.3", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.2.3"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.2.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type with extra identifier on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in taggedwithbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInTaggedwithbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("taggedwithbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("taggedwithbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.3.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.3.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.3.3", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.3.3"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.3.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type with extra identifier on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in untaggedwithoutbump branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInUntaggedwithoutbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("untaggedwithoutbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("untaggedwithoutbump", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom flat release type with extra identifier on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in untaggedwithbump branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING)
        void runUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInUntaggedwithbumpBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("untaggedwithbump");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches(".*");
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("untaggedwithbump", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-extra.5", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in master branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInMasterBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("master");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            
            command.run();

            assertEquals("master", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getReleaseType().getCollapseVersions());
            assertNull(command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertNull(command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(1, command.state().getReleaseType().getIdentifiers().size());
            assertEquals("^(master|main)$", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in alpha branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInAlphaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("alpha");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("alpha", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-alpha.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-alpha.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-alpha.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in beta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInBetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("beta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("beta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-beta.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-beta.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-beta.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in gamma branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInGammaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("gamma");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("gamma", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in delta branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInDeltaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("delta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            
            command.run();

            assertEquals("delta", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-number.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in epsilon branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInEpsilonBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("epsilon");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("epsilon", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("1.0.0-epsilon.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-epsilon.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-epsilon.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in zeta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInZetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("zeta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("zeta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("1.0.0-zeta.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-zeta.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-zeta.4", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in eta branch > yield to no new version or release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInEtaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("eta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                    "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("eta", command.state().getBranch());
            assertNull(command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("1.0.0-eta.3", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("1.0.0-eta.3"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("1.0.0-eta.3", command.state().getVersion());
        }
        
        @TestTemplate
        @DisplayName("Infer.run() inferring a custom collapsed release type with extra identifiers on repository with several branches and commits, with a commit message convention that infers bump identifier from commit messages, in theta branch > yield to new version but no new release")
        @Baseline(Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED)
        void runUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithExtraIdentifierWithInferringCommitConventionInThetaBranchTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            script.checkout("theta");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that takes the commit message as the identifier to bump, if any
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", "^major.*", "minor", "^minor.*", "patch", "^patch.*")))
                )
            );
            // add a custom release type that matches any branch
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseTypeMain", "testReleaseTypeCollapsed"),
                    List.<String>of(),
                    List.<String>of(),
                    Map.<String,ReleaseType>of(
                        "testReleaseTypeMain", new ReleaseType() {
                        {
                            setIdentifiers(List.<Identifier>of(new Identifier("extra", "5", Identifier.Position.PRE_RELEASE)));
                            setMatchBranches("^(master|main)$");    // match main and master
                        }},
                        "testReleaseTypeCollapsed", new ReleaseType() {
                        {
                            setCollapseVersions(Boolean.TRUE);
                            setCollapsedVersionQualifier("number");
                            setFilterTags("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
                            setMatchBranches(".*");  // match any branch (this is the fallback release type)
                        }}
                    )
                )
            );
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);

            command.run();

            assertEquals("theta", command.state().getBranch());
            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.1", command.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.1"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(3, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseType().getCollapseVersions());
            assertEquals("number", command.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
            assertEquals("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", command.state().getReleaseType().getFilterTags());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
            assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
            assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
            assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
            assertEquals(".*", command.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.1.0-number.1", command.state().getVersion());
        }
    }
}
