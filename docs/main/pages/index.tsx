/**
 * The site main page.
 * 
 * This page uses React components for an improved look by providing
 * engaging contents to users landing here.
 * 
 * For more on React pages inside Docusaurus see:
 * - https://docusaurus.io/docs/creating-pages
 * - https://docusaurus.io/docs/markdown-features
 */

import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import HomepageFeatures, {type FeatureItem} from '@site/src/components/HomepageFeatures';
import FeaturedResources, {type ResourceItem} from '@site/src/components/FeaturedResources';

import styles from './index.module.css';

/**
 * The featured resources to display on the page.
 */
const RESOURCES: ResourceItem[] = [
  {
    name: 'GitHub',
    environment: '',
    url: 'https://github.com/mooltiverse/toolbox',
    description: (
      <></>
    ),
    logo: '/nyx/img/github.svg',
    links: [
      {
        "title": "Releases",
        "url": "https://github.com/mooltiverse/nyx/releases/latest",
        "logo": ''
      },
      {
        "title": "Action",
        "url": "https://github.com/mooltiverse/nyx-github-action",
        "logo": ''
      }
    ]
  },
  {
    name: 'Gradle',
    environment: '',
    url: 'https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx',
    description: (
      <></>
    ),
    logo: '/nyx/img/gradle.svg',
    links: [
      {
        "title": "Plugin",
        "url": "https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx",
        "logo": ''
      }
    ]
  },
  {
    name: 'Docker',
    environment: '',
    url: 'https://hub.docker.com/repository/docker/mooltiverse/nyx',
    description: (
      <></>
    ),
    logo: '/nyx/img/docker.svg',
    links: [
      {
        "title": "Docker Hub",
        "url": "https://hub.docker.com/repository/docker/mooltiverse/nyx",
        "logo": '/nyx/img/docker.svg'
      },
      {
        "title": "GitHub",
        "url": "https://github.com/mooltiverse/nyx/pkgs/container/nyx",
        "logo": '/nyx/img/github.svg'
      }
    ]
  },
  {
    name: 'Publications',
    environment: '',
    url: '',
    description: (
      <></>
    ),
    logo: '/nyx/img/news.svg',
    links: [
      {
        "title": "Nyx, the Semantic Release Automation Tool",
        "url": "https://levelup.gitconnected.com/nyx-the-semantic-release-automation-tool-4e2dfa949f38",
        "logo": '/nyx/img/medium.svg'
      },
      {
        "title": "Semantic Release Automation with Gradle using Nyx",
        "url": "https://levelup.gitconnected.com/semantic-release-automation-with-gradle-using-nyx-ba345235a365",
        "logo": '/nyx/img/medium.svg'
      }
    ]
  },
  {
    name: 'Go',
    environment: '',
    url: 'https://github.com/mooltiverse/nyx/tree/main/src/go',
    description: (
      <></>
    ),
    logo: '/nyx/img/go.svg',
    links: [
      {
        "title": "Godoc",
        "url": "https://godocs.io/github.com/mooltiverse/nyx/src/go/nyx",
        "logo": '/nyx/img/go.svg'
      }
    ]
  },
  {
    name: 'Java',
    environment: '',
    url: 'https://github.com/mooltiverse/nyx/tree/main/src/java',
    description: (
      <></>
    ),
    logo: '/nyx/img/java.svg',
    links: [
      {
        "title": "GitHub",
        "url": "https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/",
        "logo": '/nyx/img/github.svg'
      },
      {
        "title": "Maven",
        "url": "https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/",
        "logo": '/nyx/img/maven.svg'
      },
      {
        "title": "Javadoc",
        "url": "https://javadoc.io/doc/com.mooltiverse.oss.nyx/java",
        "logo": '/nyx/img/java.svg'
      }
    ]
  },
];

/**
 * The features to display on the page.
 */
const FEATURES: FeatureItem[] = [
  {
    title: 'Semantic Release Automation',
    image: {
      src: '/nyx/img/rocket.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        For any platform and language.
      </>
    ),
  },
  {
    title: 'Semantic Versioning',
    image: {
      src: '/nyx/img/protocol.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        <a href='https://semver.org/' target='_blank'>Semantic Versioning (SemVer)</a> compliance.
      </>
    ),
  },
  {
    title: 'Conventions Support',
    image: {
      src: '/nyx/img/handshake.svg',
      width: 50.00,
      height: 50.00,
    },
    description: (
      <>
        <a href='https://www.conventionalcommits.org/' target='_blank'>Conventional Commits</a>, <a href='https://gitmoji.dev/' target='_blank'>Gitmoji</a>, <a href='https://keepachangelog.com/' target='_blank'>Keep a Changelog</a> and custom ones.
      </>
    ),
  },
  {
    title: 'Release Management',
    image: {
      src: '/nyx/img/package.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        Manage releases for GitHub and GitLab.
      </>
    ),
  },
  {
    title: 'Changelog Generation',
    image: {
      src: '/nyx/img/log.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        Create automatic changelogs for new releases.
      </>
    ),
  },
  {
    title: 'Branching Models',
    image: {
      src: '/nyx/img/workflow.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        Supports any standard workflow (i.e.
        <a href='https://nvie.com/posts/a-successful-git-branching-model/' target='_blank'>GitFlow</a>,
        <a href='https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow' target='_blank'>GitHub Flow</a>
        <a href='https://docs.gitlab.com/ee/topics/gitlab_flow.html' target='_blank'>GitLab Flow</a>
        <a href='https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow' target='_blank'>OneFlow</a>
        ) as well as custom ones.
      </>
    ),
  },
  {
    title: 'GitHub Actions',
    image: {
      src: '/nyx/img/github-actions.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        <a href='https://github.com/mooltiverse/nyx-github-action' target='_blank'>GitHub Action</a> available.
      </>
    ),
  },
  {
    title: 'Gradle Support',
    image: {
      src: '/nyx/img/gradle.svg',
      width: 100.00,
      height: 50.00,
    },
    description: (
      <>
        <a href='https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx' target='_blank'>Gradle Plugin</a> available.
      </>
    ),
  },
];

/**
 * The first block displayed on the page shows the project name, the tagline
 * and a first button to take the user to the documentation.
 * You can customize this block to change its contents and appearence.
 */
function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className={styles.heroProjectTitle}>
          {siteConfig.title}
        </Heading>
        <p className={styles.heroProjectTagline}>{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/user/quick-start/">
            User Guide
          </Link>
        </div>
      </div>
    </header>
  );
}

/**
 * Define the home page and use nested elements for the nested blocks.
 */
export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="Documentation site for ${siteConfig.title}">
      <HomepageHeader />
      <main>
        <HomepageFeatures features={FEATURES} />
        <FeaturedResources resources={RESOURCES} />
      </main>
    </Layout>
  );
}
