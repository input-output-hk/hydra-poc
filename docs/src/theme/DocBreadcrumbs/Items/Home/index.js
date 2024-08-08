import React from "react";
import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { translate } from "@docusaurus/Translate";
import styles from "./styles.module.css";
import Home from "../../../../components/icons/Home";
export default function HomeBreadcrumbItem() {
  const homeHref = useBaseUrl("/");
  return (
    <li className="breadcrumbs__item">
      <Link
        aria-label={translate({
          id: "theme.docs.breadcrumbs.home",
          message: "Home page",
          description: "The ARIA label for the home page in the breadcrumbs",
        })}
        className="breadcrumbs__link"
        href={homeHref}
      >
        <Home className={styles.breadcrumbHomeIcon} />
      </Link>
    </li>
  );
}