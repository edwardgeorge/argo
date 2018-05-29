## Swagger Auto-Generated [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) Bindings to `Argo` 

The library in `lib` provides auto-generated-from-Swagger [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) bindings to the Argo API.

Argo version: [v2.1.0](https://github.com/argoproj/argo/tree/v2.1.0)

Targeted swagger version: 2.0

OpenAPI-Specification: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md

This package depends on the unreleased [kubernetes haskell client](https://github.com/kubernetes-client/haskell) for the K8s definitions rather than replicate them in here.

As that package uses an arbitrary commit of the 2.4 branch of swagger-codegen I used a snapshot for the generation of this code: `swagger-codegen-cli-2.4.0-20180525.184426-259.jar`

The following pre-processing steps were applied to the `swagger.json` file to match deps with `kubernetes`:
```
$ sed -i -E 's/io.k8s.api.core.v1/v1/' swagger.json
$ sed -i -E 's/io.k8s.apimachinery.pkg.apis.meta.v1/v1/' swagger.json
# and finally to remove prefixes from argo models
$ sed -i -E 's/io.argoproj.workflow.v1alpha1.//' swagger.json
```

Also, `WorkflowStatus` (and its deps) where added back to `swagger.json` manually (they are omitted from the generation process in `argo`).
