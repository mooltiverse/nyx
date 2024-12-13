package com.mooltiverse.oss.nyx.version;

/**
 * The implementation of a Maven compliant version.
 * <br>
 * TODO: implement this class as per https://github.com/mooltiverse/nyx/issues/4. As of now this class is just a placeholder.
 */
abstract class MavenVersion extends Version implements Comparable<MavenVersion> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Default constructor hidden on purpose.
     */
    private MavenVersion() {
        super();
    }
}
