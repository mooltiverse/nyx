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
package com.mooltiverse.oss.nyx.core;

//import static org.junit.jupiter.api.Assertions.*;
//import static org.junit.jupiter.api.Assumptions.*;
//import static org.junit.jupiter.params.provider.Arguments.arguments;

//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.Comparator;
//import java.util.Collections;
//import java.util.Iterator;
//import java.util.List;
//import java.util.ListIterator;
//import java.util.Random;
//import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.params.ParameterizedTest;
//import org.junit.jupiter.params.provider.Arguments;
//import org.junit.jupiter.params.provider.EmptySource;
//import org.junit.jupiter.params.provider.MethodSource;
//import org.junit.jupiter.params.provider.NullSource;
//import org.junit.jupiter.params.provider.NullAndEmptySource;

@DisplayName("Nyx")
public class NyxTest {
    /**
     * Test Nyx using any version
     */
    @Nested
    @DisplayName("Nyx<? extends Version>")
    class NyxAnyVersion {
    }

    /**
     * Test Nyx using SemanticVersion
     */
    @Nested
    @DisplayName("Nyx<SemanticVersion>")
    class NyxSemanticVersion {
    }
}