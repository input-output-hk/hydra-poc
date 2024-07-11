module.exports = {
  userDocumentation: [
    "index",
    "protocol-overview",
    "known-issues",
    {
      type: "html",
      value: "<small><b>Tutorials</b></small>",
      defaultStyle: true,
      className: "sidebar-header",
    },
    "getting-started",
    "tutorial/index",
    {
      type: "html",
      value: "<small><b>Documentation</b></small>",
      defaultStyle: true,
      className: "sidebar-header",
    },
    "installation",
    "configuration",
    {
      type: "category",
      label: "How to ...",
      // collapsed: true,
      // collapsible: true,
      items: [
        {
          type: "autogenerated",
          dirName: "how-to",
        },
      ],
    },
    "faqs",
    {
      type: "html",
      value: "<small><b>Reference</b></small>",
      defaultStyle: true,
      className: "sidebar-header",
    },
    {
      type: "link",
      href: "https://github.com/cardano-scaling/hydra/releases",
      label: "Release notes",
    },
    {
      type: "link",
      href: "/api-reference",
      label: "API reference",
    },
    "api-behavior",
    {
      type: "link",
      href: "/benchmarks",
      label: "Benchmarks",
    },
  ],

  developerDocumentation: [
    "dev/index",
    {
      type: "doc",
      id: "dev/specification",
      label: "Specification",
    },
    {
      type: "category",
      link: { type: "doc", id: "dev/architecture/index" },
      label: "Architecture",
      items: ["dev/architecture/networking"],
    },
    "dev/rollbacks/index",
    {
      type: "html",
      value: "<small><b>Background</b></small>",
      defaultStyle: true,
      className: "sidebar-header",
    },
    "dev/scalability",
    "dev/layer-two",
    {
      type: "html",
      value: "<small><b>Reference</b></small>",
      defaultStyle: true,
      className: "sidebar-header",
    },
    "dev/haskell-packages",
    {
      type: "link",
      href: "/adr",
      label: "Architecture Decision Records",
    },
  ],
};
