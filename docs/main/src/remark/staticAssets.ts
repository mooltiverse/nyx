/*
 * Remark plugin that serves the brand assets under their own names.
 *
 * Docusaurus turns every local image or file linked from Markdown into a
 * Webpack asset, published under '/assets/...' with a content hash in its
 * name. Brand assets must instead keep stable URLs, so that others can
 * reference them by name. They're published as static files from the
 * 'assets' static directory (see 'staticDirectories' in docusaurus.config.ts),
 * while the Markdown sources keep the relative paths used while authoring
 * them (i.e. 'secondary/<name>.svg').
 *
 * This plugin rewrites the relative image and link URLs that point to a file
 * in the static directory to 'pathname:///<path>', which Docusaurus leaves
 * untouched. URLs that don't match any file are left as they are, so the
 * broken link and image checks still apply.
 *
 * It must be registered among the 'beforeDefaultRemarkPlugins', to run before
 * Docusaurus processes images and links.
 * 
 * This is a custom plugin, not available in the Docusaurus ecosystem.
 */
import fs from 'fs';
import path from 'path';

export type StaticAssetsOptions = {
  // The directory with the Markdown sources, as the 'path' of the docs plugin
  docsDir: string;
  // The static directory where the assets are published from
  assetsDir: string;
};

export default function staticAssets(options: StaticAssetsOptions) {
  const docsDir = path.resolve(options.docsDir);
  const assetsDir = path.resolve(options.assetsDir);

  function rewrite(url: string, filePath: string): string | undefined {
    // Only relative URLs: skip anchors, absolute paths and URLs with a scheme
    if (!url || url.startsWith('#') || url.startsWith('/') || /^[a-z][a-z0-9+.-]*:/i.test(url)) {
      return undefined;
    }
    const pathname = decodeURIComponent(url.split(/[?#]/)[0]);
    const relativePath = path.relative(docsDir, path.resolve(path.dirname(filePath), pathname));
    if (relativePath.startsWith('..') || path.isAbsolute(relativePath)) {
      return undefined;
    }
    const assetPath = path.join(assetsDir, relativePath);
    if (!fs.existsSync(assetPath) || !fs.statSync(assetPath).isFile()) {
      return undefined;
    }
    return `pathname:///${relativePath.split(path.sep).join('/')}${url.slice(url.split(/[?#]/)[0].length)}`;
  }

  function visit(node: any, filePath: string): void {
    if ((node.type === 'image' || node.type === 'link' || node.type === 'definition') && typeof node.url === 'string') {
      const url = rewrite(node.url, filePath);
      if (url !== undefined) {
        node.url = url;
      }
    }
    if (Array.isArray(node.children)) {
      node.children.forEach((child: any) => visit(child, filePath));
    }
  }

  return (tree: any, file: any): void => {
    if (file?.path) {
      visit(tree, file.path);
    }
  };
}
