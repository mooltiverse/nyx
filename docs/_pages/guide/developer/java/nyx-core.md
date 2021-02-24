---
title: Nyx Core
layout: single
toc: true
permalink: /guide/developer/java/nyx-core/
---

[![Maven Central](https://img.shields.io/maven-central/v/com.mooltiverse.oss.nyx/core.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.mooltiverse.oss.nyx%22%20AND%20a:%22core%22) [![javadoc](https://javadoc.io/badge2/com.mooltiverse.oss.nyx/core/javadoc.svg)](https://javadoc.io/doc/com.mooltiverse.oss.nyx/core)

TODO: check the links to external sites (Maven and Javadoc.io)
{: .notice--warning}

You can use the core Nyx library to embed it into your project and use all or some of its features. The `com.mooltiverse.oss.nyx` package brings the [`Nyx`](https://javadoc.io/doc/com.mooltiverse.oss.nyx/core/latest/com/mooltiverse/oss/nyx/Nyx.html){:target="_blank"} class that is the entry point for all the available commands and features.

When using the library you may benefit from knowing some insights about the implementation available in [this]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/index.md %}) section.
{: .notice--info}

## Get the library

### Manual download
You can download the jar file directly from the [Maven Central](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/core/){:target="_blank"} repository or by using the [Maven Central repository search engine](https://search.maven.org/artifact/com.mooltiverse.oss.nyx/core){:target="_blank"}. The [GitHub Packages](https://github.com/mooltiverse/nyx/packages/){:target="_blank"} repository is the other source where you can get it.

In order to also download the correct dependencies you should use one of the automatic tools below. If you really want to download manually take a look at the [`pom`](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/core/{{ site.data.nyx.version }}/core-{{ site.data.nyx.version }}.pom){:target="_blank"} to see the dependencies to download.
{: .notice--info}

### Using Maven
When using Maven just add the following dependency to your `POM`:

```xml
<dependency>
  <groupId>com.mooltiverse.oss.nyx</groupId>
  <artifactId>core</artifactId>
  <version>{{ site.data.nyx.version }}</version>
</dependency>
```

Your local Maven setup will likely use the [Maven Central](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/core/) repository by default but if you like to use the [GitHub Packages](https://github.com/mooltiverse/nyx/packages/) repository you can follow [these instructions](https://help.github.com/en/packages/using-github-packages-with-your-projects-ecosystem/configuring-apache-maven-for-use-with-github-packages).
{: .notice--info}

### Using Ivy
When using Ivy just add the following dependency:

```xml
<dependency org="com.mooltiverse.oss.nyx" name="core" rev="{{ site.data.nyx.version }}" />
```

### Using Gradle

If you're using the Groovy DLS add this to the dependencies your script:

```groovy
implementation 'com.mooltiverse.oss.nyx:core:{{ site.data.nyx.version }}'
```

while if you're using the Kotlin DSL use this dependency:

```kotlin
implementation("com.mooltiverse.oss.nyx:core:{{ site.data.nyx.version }}")
```

## API docs

Thanks to [javadoc.io](https://javadoc.io/) you can browse the Javadoc API at [this URL](https://javadoc.io/doc/com.mooltiverse.oss.nyx/core).

## Using the library

Using the library is simple. Start from the [`Nyx`](https://javadoc.io/doc/com.mooltiverse.oss.nyx/core/latest/com/mooltiverse/oss/nyx/Nyx.html){:target="_blank"} class as an entry point, optionally add your custom configuration and use it as in the following example.

```java
import com.mooltiverse.oss.nyx.Nyx;

public class Test {
    static void main(String[] args){
        // TODO: add a code example here
    }
}
```

### Configuration

TODO: write this section
{: .notice--warning}

### Logging

TODO: write this section
{: .notice--warning}
