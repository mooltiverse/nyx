import clsx from 'clsx';
import Heading from '@theme/Heading';
import Link from '@docusaurus/Link';
import styles from './styles.module.css';

export interface LinkProps {
    title: string;
    url: string;
    logo: string;
}

export type ResourceItem = {
    name: string;
    environment: string;
    url: string;
    description: JSX.Element;
    logo: string;
    links?: LinkProps[];
};

function Resource({name, environment, url, description, logo, links}: ResourceItem): JSX.Element {
    return (
      <div className={clsx('card', styles.resource)}>
        <div className="card__header">
          <div className="avatar">
            <Link className={clsx(styles.resourceMeta)} to={url}>
              <img
                alt={name}
                className="avatar__photo"
                src={logo}
                width="48"
                height="48"
                loading="lazy"
              />
            </Link>
            <div className={clsx('avatar__intro', styles.resourceMeta)}>
              <Link className={clsx(styles.resourceMeta)} to={url}>
                <strong className="avatar__name">{name}</strong>
              </Link>
              <span>{environment}</span>
            </div>
          </div>
        </div>
  
        <div className={clsx('card__body', styles.resource)}>{description}</div>
  
        {links && links.length > 0 && (
          <div className={styles.linksWrapper}>
            <div className={styles.links}>
              {links.map((link, index) => (
                
                <Link className={clsx(styles.link)} to={link.url}>
                  <img 
                    alt="" 
                    className={styles.favicon}
                    src={link.logo && link.logo || logo}
                />
                {link.title}
                </Link>
              ))}
            </div>
          </div>
        )}
      </div>
    );
  }

export default function FeaturedResources({ resources }) {
    return (
        <div className={clsx(styles.section, styles.sectionAlt)}>
        <div className="container">
            <Heading as="h2" className={clsx('margin-bottom--lg', 'text--center')}>
            Featured Resources
            </Heading>
            <div className={clsx('row', styles.resourcesSection)}>
                {resources.map((resource, i) => (
                <div className="col col--4" key={i}>
                    <Resource {...resource} />
                </div>
            ))}
            </div>
        </div>
        </div>
    );
}