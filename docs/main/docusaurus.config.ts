import {themes as prismThemes} from 'prism-react-renderer';
// Import the custom project variables
import {project} from './src/data/projectVariables';

export default {
  // These are the site global metadata attributes
  title: 'Nyx',
  tagline: 'The one stop semantic release tool',
  favicon: '/nyx/img/favicon.ico',
  url: 'https://mooltiverse.github.io/',
  baseUrl: '/nyx/',
  organizationName: 'mooltiverse',
  projectName: 'nyx',

  // These attributes define the behavior when encountering broken links.
  // Allowed values are: 'ignore' | 'log' | 'warn' | 'throw'.
  // 'throw' means 'raise an error', which is the safest to avoid publishing
  // broken links.
  onBrokenLinks: 'throw',
  onBrokenAnchors: 'throw',
  onBrokenMarkdownLinks: 'throw',
  onDuplicateRoutes: 'throw',

  // Configure the site internationalization.
  // English is just enough in most cases.
  // For more see:
  // - https://docusaurus.io/docs/i18n/introduction
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  // Non Docusaurus fields
  customFields: {
    // When the GOOGLE_CLIENT_ID environment variable is set authentication
    // will be enabled using Google SSO.
    googleClientId: process.env.GOOGLE_CLIENT_ID,
  },

  // This is the configuration for themes.
  themes: [
    [
      '@docusaurus/theme-classic',
      {
        // Configuration for the 'theme-classic' plugin.
        // For more on the configuration of this plugin see:
        // - https://docusaurus.io/docs/api/themes/configuration
        // - https://docusaurus.io/docs/api/themes/@docusaurus/theme-classic
        customCss: './src/css/custom.css',
      },
    ],
    [
      require.resolve("docusaurus-lunr-search"),
      {
        // Configuration for the 'docusaurus-lunr-search' plugin.
        // For more on the configuration of this plugin see:
        // - https://github.com/praveenn77/docusaurus-lunr-search
        //
        // NOTE: The search box does not display when the site is rendered locally
        // (sometimes it displays but doesn't work and keeps showing a 'loading...'
        // message).
        // Only regular site builds show the search box.
        //
        // NOTE: in order to avoid build tasks to install the plugin locally,
        // i.e. by running 'npm install --save docusaurus-lunr-search',
        // we have added the plugin in the 'dependencies' section within the
        // 'package.json' file so that when we run the 'npm install' command
        // from Gradle scripts this plugin is installed along with all other
        // Node packages.
        //
        // NOTE: according to the docs this plugin should be configured under
        // the 'plugins' block but if we do so it just never displays.
        // Thanks to this issue about another local search plugin at
        // https://github.com/easyops-cn/docusaurus-search-local/issues/114#issuecomment-1236191485
        // we tried moving the configuration here under the 'themes' block
        // and it works, although we still don't know why.
        highlightResult: true,
      },
    ],
  ],

  // This is the configuration for plugins.
  plugins: [
    [
      '@docusaurus/plugin-content-pages',
      {
        // Configuration for the 'plugin-content-pages' plugin.
        // For more on the configuration of this plugin see:
        // - https://docusaurus.io/docs/api/plugins/@docusaurus/plugin-content-pages

        // Path to data on filesystem relative to site dir.
        // Components in this directory will be automatically converted to pages.
        path: 'pages',
        // Only for Markdown pages. Base URL to edit your site.
        // The final URL is computed by 'editUrl' + 'relativePostPath'.
        editUrl: 'https://github.com/mooltiverse/nyx/tree/main/docs/main/',
      },
    ],
    [
      '@docusaurus/plugin-content-docs',
      {
        // Configuration for the 'plugin-content-docs' plugin.
        // For more on the configuration of this plugin see:
        // - https://docusaurus.io/docs/api/plugins/@docusaurus/plugin-content-docs

        // Path to the docs content directory on the file system, relative to site directory.
        path: 'docs',
        // Base URL to edit your site. The final URL is computed by 'editUrl' + 'relativeDocPath'.
        editUrl: 'https://github.com/mooltiverse/nyx/tree/main/docs/main/',
        // Path to sidebar configuration. Use 'false' to disable sidebars,
        // or 'undefined' to create a fully autogenerated sidebar.
        sidebarPath: './sidebars.ts',
        // Whether sidebar categories are collapsible by default.
        sidebarCollapsible: true,
      },
    ],
    [
      '@docusaurus/plugin-sitemap',
      {
        // Configuration for the 'plugin-sitemap' plugin.
        // For more on the configuration of this plugin see:
        // - https://docusaurus.io/docs/api/plugins/@docusaurus/plugin-sitemap
      }
    ],
  ],

  // This is the configuration for presets.
  presets: [
    // This configuration doesn't use presets.
  ],

  // This is the configuration for the theme.
  themeConfig: {
    // For more on the configuration of the theme see:
    // - https://docusaurus.io/docs/api/themes/configuration

    // This is the meta image URL for the site, relative to the site's "static" directory.
    // If the 'baseUrl' global attribute is other than '/', this path must also include
    // the 'baseUrl' as prefix. See https://docusaurus.io/docs/static-assets for more.
    image: '/nyx/img/logo.png',
    // Configuration for the announcement bar.
    // Uncomment this block and edit its attributes in case you need
    // to display an announcement on your site.
    // For more see:
    // - https://docusaurus.io/docs/api/themes/configuration#announcement-bar
    /*announcementBar: {
      content: 'This is the text of the announcement. You can use HTML tags here like <a target="_blank" rel="noopener noreferrer" href="#">this one</a>',
      textColor: '#091E42',
      backgroundColor: '#fafbfc',
      isCloseable: true,
    },*/
    // Configuration for the docs plugin within the theme.
    // This plugin also uses the sidebars defined in the 'sidebars.ts' file.
    // For more see:
    // - https://docusaurus.io/docs/api/themes/configuration#docs
    // - https://docusaurus.io/docs/sidebar
    docs: {
      versionPersistence: 'localStorage',
      sidebar: {
        hideable: true,
        autoCollapseCategories: false,
      },
    },
    // Configuration for the navbar within the theme.
    // For more see:
    // - https://docusaurus.io/docs/api/themes/configuration#navbar
    navbar: {
      title: 'Nyx',
      logo: {
        alt: 'Logo',
        src: '/nyx/img/icon.svg',
      },
      // Configuration for the navbar items displayed on top of the site.
      // Note that internal links use the 'to' attribute to the local route,
      // with the 'baseUrl' will be automatically prepended to this value,
      // while external links use 'href'.
      // For more see:
      // - https://docusaurus.io/docs/api/themes/configuration#navbar-items
      items: [
        {
          // See the 'guideSidebar' in 'sidebars.ts' for the definition of this sidebar.
          type: 'docSidebar',
          label: 'User',
          sidebarId: 'userGuideSidebar',
          position: 'left',
        },
        {
          // See the 'developerGuideSidebar' in 'sidebars.ts' for the definition of this sidebar.
          type: 'docSidebar',
          label: 'Developer',
          sidebarId: 'developerGuideSidebar',
          position: 'left',
        },
        {
          // See the 'resourcesSidebar' in 'sidebars.ts' for the definition of this sidebar.
          type: 'docSidebar',
          label: 'Resources',
          sidebarId: 'resourcesSidebar',
          position: 'left',
        },
        {
          // Display the search box using the plugin configured above.
          // For more see:
          // - https://docusaurus.io/docs/api/themes/configuration#navbar-search
          //
          // NOTE: The search box does not display when the site is rendered locally.
          // Only regular site builds show the search box.
          type: 'search',
          position: 'right',
        },
      ],
    },
    // Configuration for the footer within the theme.
    // For more see:
    // - https://docusaurus.io/docs/api/themes/configuration#footer-1
    footer: {
      style: 'dark',
      // Configuration for the footer links.
      // When using 'items' within 'links' elements are organized into columns.
      // Note that internal links use the 'to' attribute to the local route,
      // with the 'baseUrl' will be automatically prepended to this value,
      // while external links use 'href'.
      // For more see:
      // - https://docusaurus.io/docs/api/themes/configuration#footer-links
      links: [
        {
          title: 'Resources',
          items: [
            {
              label: 'Releases',
              to: 'https://github.com/mooltiverse/nyx/releases/latest',
            },
            {
              label: 'Docker Images (Docker Hub)',
              to: 'https://hub.docker.com/repository/docker/mooltiverse/nyx',
            },
            {
              label: 'Docker Images (GitHub)',
              to: 'https://github.com/mooltiverse/nyx/pkgs/container/nyx',
            },
            {
              label: 'Gradle Plug-In',
              to: 'https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx',
            },
            {
              label: 'Go Docs',
              to: 'https://godocs.io/github.com/mooltiverse/nyx/src/go/nyx',
            },
            {
              label: 'Javadocs',
              to: 'https://javadoc.io/doc/com.mooltiverse.oss.nyx/java',
            },
          ],
        },
        {
          title: 'Support',
          items: [
            {
              label: 'Examples',
              to: 'docs/resources/examples/',
            },
            {
              label: 'FAQ',
              to: 'docs/resources/faq/',
            },
            {
              label: 'Troubleshooting',
              to: 'docs/resources/troubleshooting/',
            },
            {
              label: 'Issues',
              href: 'https://github.com/mooltiverse/nyx/issues',
            },
            {
              label: 'Discussions',
              href: 'https://github.com/mooltiverse/nyx/discussions',
            },
          ],
        },
        {
          title: 'About',
          items: [
            {
              label: 'Release Notes',
              href: 'https://github.com/mooltiverse/nyx/releases',
            },
            {
              label: 'License',
              href: 'https://github.com/mooltiverse/nyx/blob/main/LICENSE.md',
            },
            {
              label: 'Contributing',
              href: 'https://github.com/mooltiverse/nyx/blob/main/CONTRIBUTING.md',
            },
            {
              label: 'Code of Conduct',
              href: 'https://github.com/mooltiverse/nyx/blob/main/CODE_OF_CONDUCT.md',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} <a target="_blank" href="https://www.mooltiverse.com/">Mooltiverse</a>. Version ${project.version}. Built with <a target="_blank" href="https://docusaurus.io/">Docusaurus</a>.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      // Prism supports a limited set of languages by default. Here we extend
      // them to a broader range.
      // For more see:
      // - https://docusaurus.io/docs/markdown-features/code-blocks#supported-languages
      // - https://prismjs.com/#supported-languages
      // - https://github.com/FormidableLabs/prism-react-renderer/blob/master/packages/generate-prism-languages/index.ts#L9
      additionalLanguages: [
        'actionscript',
        'applescript',
        'asciidoc', //'adoc',
        'awk', //'gawk',
        'bash', //'sh', 'shell',
        'batch',
        'bison',
        'bnf', //'rbnf',
        'c',
        'clojure',
        'cmake',
        'cpp',
        'csharp', //'cs', 'dotnet',
        'css',
        'csv',
        'dart',
        'diff',
        //'django', //'jinja2',
        'dns-zone-file', //'dns-zone',
        'docker', //'dockerfile',
        'dot', //'gv',
        'ebnf',
        'eiffel',
        'elixir',
        'erlang',
        'excel-formula', //'xls', 'xlsx',
        'firestore-security-rules',
        'fsharp',
        'git',
        'go',
        'go-module', //'go-mod',
        'gradle',
        'graphql',
        'groovy',
        //'handlebars', //'hbs', 'mustache',
        'hcl',
        'hlsl',
        'http',
        'icon',
        'icu-message-format',
        'ignore', //'gitignore', 'hgignore', 'npmignore',
        'ini',
        'java',
        'javadoc',
        'javadoclike',
        'javascript', //'js',
        'javastacktrace',
        'jq',
        'js-extras',
        'js-templates',
        'jsdoc',
        'json', //'webmanifest',
        'json5',
        'jsonp',
        'jsstacktrace',
        'jsx',
        'kotlin', //'kt', 'kts',
        'latex', //'tex', 'context',
        'makefile',
        'markdown', //'md',
        'markup', //'html', 'xml', 'svg', 'mathml', 'ssml', 'atom', 'rss',
        'markup-templating',
        'mermaid',
        'mongodb',
        'nginx',
        'objectivec', //'objc',
        'perl',
        'php',
        'php-extras',
        'phpdoc',
        'plsql',
        'powerquery', //'pq', 'mscript',
        'powershell',
        'properties',
        'protobuf',
        'puppet',
        'python', //'py',
        'qsharp', //'qs',
        'r',
        'reason',
        'regex',
        'ruby', //'rb',
        'rust',
        'scheme',
        'smarty',
        //'shell-session', //'sh-session', 'shellsession'
        //'sparql', //'rq',
        'splunk-spl',
        'sql',
        'swift',
        'tcl',
        'textile',
        'toml',
        //'typescript', //'ts',
        'tsx',
        'tt2',
        'typoscript', //'tsconfig',
        'uri', //'url',
        'vim',
        'wasm',
        'wiki',
        'xml-doc',
        'yaml', //'yml',
      ],
    },
  },
};
