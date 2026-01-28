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
    url: 'https://github.com/mooltiverse/nyx',
    description: (
      <></>
    ),
    image: {
      src: 'mdi:github',
      width: 200.00,
      height: 100.00,
      color: 'Black',
    },
    links: [
      {
        "title": "Releases",
        "url": "https://github.com/mooltiverse/nyx/releases/latest",
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
    image: {
      src: 'material-icon-theme:gradle',
      width: 200.00,
      height: 100.00,
      color: 'Black',
    },
    links: [
      {
        "title": "Plugin",
        "url": "https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx",
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
    image: {
      src: 'mdi:docker',
      width: 200.00,
      height: 100.00,
      color: 'DodgerBlue',
    },
    links: [
      {
        "title": "Docker Hub",
        "url": "https://hub.docker.com/repository/docker/mooltiverse/nyx",
      },
      {
        "title": "GitHub",
        "url": "https://github.com/mooltiverse/nyx/pkgs/container/nyx",
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
    image: {
      src: 'fluent-color:news-16',
      width: 200.00,
      height: 100.00,
      color: '',
    },
    links: [
      {
        "title": "Nyx, the Semantic Release Automation...",
        "url": "https://levelup.gitconnected.com/nyx-the-semantic-release-automation-tool-4e2dfa949f38",
        image: {
          src: 'mingcute:medium-fill',
          width: 200.00,
          height: 100.00,
          color: 'Black',
        },
      },
      {
        "title": "Semantic Release Automation...",
        "url": "https://levelup.gitconnected.com/semantic-release-automation-with-gradle-using-nyx-ba345235a365",
        image: {
          src: 'mingcute:medium-fill',
          width: 200.00,
          height: 100.00,
          color: 'Black',
        },
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
    image: {
      src: 'material-icon-theme:go',
      width: 200.00,
      height: 100.00,
      color: '',
    },
    links: [
      {
        "title": "Godoc",
        "url": "https://godocs.io/github.com/mooltiverse/nyx/src/go/nyx",
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
    image: {
      src: 'devicon:java',
      width: 200.00,
      height: 100.00,
      color: '',
    },
    links: [
      {
        "title": "GitHub",
        "url": "https://github.com/orgs/mooltiverse/packages?repo_name=nyx",
      },
      {
        "title": "Javadoc",
        "url": "https://javadoc.io/doc/com.mooltiverse.oss.nyx/java",
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
      src: 'material-symbols:rocket-launch',
      width: 100.00,
      height: 50.00,
      color: 'OrangeRed',
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
      src: 'mdi:git-repository',
      width: 100.00,
      height: 50.00,
      color: 'DodgerBlue',
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
      src: 'hugeicons:agreement-02',
      width: 100.00,
      height: 50.00,
      color: 'Green',
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
      src: 'fluent-mdl2:release-gate',
      width: 100.00,
      height: 50.00,
      color: 'Orange',
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
      src: 'material-icon-theme:log',
      width: 100.00,
      height: 50.00,
      color: '',
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
      src: 'qlementine-icons:version-control-16',
      width: 100.00,
      height: 50.00,
      color: 'Cyan',
    },
    description: (
      <>
        Supports any standard workflow (i.e. <a href='https://nvie.com/posts/a-successful-git-branching-model/' target='_blank'>GitFlow</a>, <a href='https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow' target='_blank'>GitHub Flow</a>, <a href='https://docs.gitlab.com/ee/topics/gitlab_flow.html' target='_blank'>GitLab Flow</a>, <a href='https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow' target='_blank'>OneFlow</a>) as well as custom ones.
      </>
    ),
  },
  {
    title: 'GitHub Actions',
    image: {
      src: 'devicon:githubactions',
      width: 100.00,
      height: 50.00,
      color: '',
    },
    description: (
      <>
        <a href='https://mooltiverse.github.io/nyx/docs/user/quick-start/github-action' target='_blank'>GitHub Action</a> available.
      </>
    ),
  },
  {
    title: 'Gradle Support',
    image: {
      src: 'material-icon-theme:gradle',
      width: 100.00,
      height: 50.00,
      color: '',
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
