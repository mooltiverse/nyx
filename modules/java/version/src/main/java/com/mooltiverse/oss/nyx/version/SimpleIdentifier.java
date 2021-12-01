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
package com.mooltiverse.oss.nyx.version;

/**
 * An identifier modelling a simple {@link Object} value like {@link String} or {@link Integer}.
 * 
 * In <a href="https://semver.org/">Semantic Versioning 2.0.0</a> parlance, an identifier can be
 * numeric or alphanumeric.
 */
abstract class SimpleIdentifier extends Identifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Default contstructor, protected on purpose.
     */
    protected SimpleIdentifier() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 31 * getValue().hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (this == obj)
            return true;
        if (!this.getClass().isInstance(obj))
            return false;
        return getValue().equals(this.getClass().cast(obj).getValue());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return getValue().toString();
    }
}
