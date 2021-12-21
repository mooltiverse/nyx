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
package com.mooltiverse.oss.nyx.services;

/**
 * An abstraction over services.
 */
public interface Service {
    /**
     * Safely checks if the underlying implementation supports the given operation. If this
     * method returns {@code true} then the underlying class will not raise any
     * {@link UnsupportedOperationException} when invoking the specific methods.
     * 
     * @param feature the feature to check for support.
     * 
     * @return {@code true} if the operation is supported, {@code false} otherwise
     */
    public boolean supports(Feature feature);

    /**
     * These are the constants representing the features that may or may not be supported by services.
     * <br>
     * When invoking an operation method against a service and the operation is not supported then
     * the method throws an {@link UnsupportedOperationException}.
     * <br>
     * In order to safely know if an operation is supported you can query the service object via
     * the {@code supports} method.
     */
    public enum Feature {
        /**
         * When this feature is supported then the implementation class implements the {@link GitHostingService} interface
         * (so it can be safely cast to it) and the service specific methods can be safely invoked without an
         * {@link UnsupportedOperationException} being thrown.
         */
        GIT_HOSTING(),

        /**
         * When this feature is supported then the implementation class implements the {@link ReleaseService} interface
         * (so it can be safely cast to it) and the service specific methods can be safely invoked without an
         * {@link UnsupportedOperationException} being thrown.
         */
        RELEASES(),
        
        /**
         * When this feature is supported then the implementation class implements the {@link UserService} interface
         * (so it can be safely cast to it) and the service specific methods can be safely invoked without an
         * {@link UnsupportedOperationException} being thrown.
         */
        USERS();
    }
}