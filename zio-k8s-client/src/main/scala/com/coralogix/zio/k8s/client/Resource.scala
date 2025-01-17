package com.coralogix.zio.k8s.client

import com.coralogix.zio.k8s.client.model.{
  FieldSelector,
  K8sNamespace,
  LabelSelector,
  ListResourceVersion,
  PropagationPolicy,
  Reseted,
  TypedWatchEvent
}
import com.coralogix.zio.k8s.model.pkg.apis.meta.v1.{ DeleteOptions, Preconditions, Status }
import sttp.model.StatusCode
import zio.{ IO, Schedule, ZIO }
import zio.clock.Clock
import zio.duration.{ durationInt, Duration }
import zio.stream.{ Stream, ZStream }

/** Generic interface for working with Kubernetes resources
  *
  * This interface supports both namespaced and cluster resources. For more type safe variants check
  * [[NamespacedResource]] and [[ClusterResource]].
  *
  * @tparam T
  *   Resource type
  */
trait Resource[T] {

  /** A paginated query of all resources with filtering possibilities
    * @param namespace
    *   Constraint the query to a given namespace. If None, results returned from all namespaces.
    * @param chunkSize
    *   Number of items to return per HTTP request
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @param resourceVersion
    *   Control the returned resources' version.
    * @return
    *   A stream of resources
    */
  def getAll(
    namespace: Option[K8sNamespace],
    chunkSize: Int = 10,
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None,
    resourceVersion: ListResourceVersion = ListResourceVersion.MostRecent
  ): Stream[K8sFailure, T]

  /** Watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * This function requires the user to control the starting resourceVersion and to restart the
    * watch stream when the server closes the connection.
    *
    * For a more convenient variant check [[watchForever]].
    *
    * @param namespace
    *   Constraint the watched resources by their namespace. If None, all namespaces will be
    *   watched.
    * @param resourceVersion
    *   Last known resource version
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watch(
    namespace: Option[K8sNamespace],
    resourceVersion: Option[String],
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): Stream[K8sFailure, TypedWatchEvent[T]]

  /** Infinite watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * The underlying implementation takes advantage of Kubernetes watch bookmarks.
    *
    * @param namespace
    *   Constraint the watched resources by their namespace. If None, all namespaces will be
    *   watched.
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watchForever(
    namespace: Option[K8sNamespace],
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): ZStream[Clock, K8sFailure, TypedWatchEvent[T]] =
    ZStream.succeed(Reseted[T]()) ++ watch(namespace, None, fieldSelector, labelSelector)
      .retry(Schedule.recurWhileEquals(Gone))

  /** Get a resource by its name
    * @param name
    *   Name of the resource
    * @param namespace
    *   Namespace. For namespaced resources it must be Some. For cluster resources, it must be None.
    * @return
    *   Returns the current version of the resource
    */
  def get(name: String, namespace: Option[K8sNamespace]): IO[K8sFailure, T]

  /** Creates a new resource
    * @param newResource
    *   The new resource to define in the cluster
    * @param namespace
    *   Namespace. For namespaced resources it must be Some. For cluster resources, it must be None.
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the created resource as it was returned from Kubernetes
    */
  def create(
    newResource: T,
    namespace: Option[K8sNamespace],
    dryRun: Boolean = false
  ): IO[K8sFailure, T]

  /** Replaces an existing resource selected by its name
    * @param name
    *   Name of the resource
    * @param updatedResource
    *   The new value of the resource
    * @param namespace
    *   Namespace. For namespaced resources it must be Some. For cluster resources, it must be None.
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the updated resource as it was returned from Kubernetes
    */
  def replace(
    name: String,
    updatedResource: T,
    namespace: Option[K8sNamespace],
    dryRun: Boolean = false
  ): IO[K8sFailure, T]
}

/** Generic interface for working with namespaced Kubernetes resources.
  *
  * More type safe than [[Resource]] as it requires passing a namespace where necessary.
  *
  * @tparam T
  *   Resource type
  */
trait NamespacedResource[T] {

  /** A more generic interface for the same resource
    */
  val asGenericResource: Resource[T]

  /** A paginated query of all resources with filtering possibilities
    * @param namespace
    *   Constraint the query to a given namespace. If None, results returned from all namespaces.
    * @param chunkSize
    *   Number of items to return per HTTP request
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @param resourceVersion
    *   Control the returned resources' version.
    * @return
    *   A stream of resources
    */
  def getAll(
    namespace: Option[K8sNamespace],
    chunkSize: Int = 10,
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None,
    resourceVersion: ListResourceVersion = ListResourceVersion.MostRecent
  ): Stream[K8sFailure, T] =
    asGenericResource.getAll(namespace, chunkSize, fieldSelector, labelSelector, resourceVersion)

  /** Watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * This function requires the user to control the starting resourceVersion and to restart the
    * watch stream when the server closes the connection.
    *
    * For a more convenient variant check [[watchForever]].
    *
    * @param namespace
    *   Constraint the watched resources by their namespace. If None, all namespaces will be
    *   watched.
    * @param resourceVersion
    *   Last known resource version
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watch(
    namespace: Option[K8sNamespace],
    resourceVersion: Option[String],
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): Stream[K8sFailure, TypedWatchEvent[T]] =
    asGenericResource.watch(namespace, resourceVersion, fieldSelector, labelSelector)

  /** Infinite watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * The underlying implementation takes advantage of Kubernetes watch bookmarks.
    *
    * @param namespace
    *   Constraint the watched resources by their namespace. If None, all namespaces will be
    *   watched.
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watchForever(
    namespace: Option[K8sNamespace],
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): ZStream[Clock, K8sFailure, TypedWatchEvent[T]] =
    asGenericResource.watchForever(namespace, fieldSelector, labelSelector)

  /** Get a resource by its name
    * @param name
    *   Name of the resource
    * @param namespace
    *   Namespace of the resource
    * @return
    *   Returns the current version of the resource
    */
  def get(name: String, namespace: K8sNamespace): IO[K8sFailure, T] =
    asGenericResource.get(name, Some(namespace))

  /** Creates a new resource
    * @param newResource
    *   The new resource to define in the cluster.
    * @param namespace
    *   Namespace of the resource.
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the created resource as it was returned from Kubernetes
    */
  def create(newResource: T, namespace: K8sNamespace, dryRun: Boolean = false): IO[K8sFailure, T] =
    asGenericResource.create(newResource, Some(namespace), dryRun)

  /** Replaces an existing resource selected by its name
    * @param name
    *   Name of the resource
    * @param updatedResource
    *   The new value of the resource
    * @param namespace
    *   Namespace of the resource
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the updated resource as it was returned from Kubernetes
    */
  def replace(
    name: String,
    updatedResource: T,
    namespace: K8sNamespace,
    dryRun: Boolean = false
  ): IO[K8sFailure, T] =
    asGenericResource.replace(name, updatedResource, Some(namespace), dryRun)
}

/** Generic interface for working with Kubernetes cluster resources.
  *
  * More type safe than [[Resource]] as it does not allow passing a namespace.
  *
  * @tparam T
  *   Resource type
  */
trait ClusterResource[T] {

  /** A more generic interface for the same resource
    */
  val asGenericResource: Resource[T]

  /** A paginated query of all resources with filtering possibilities
    * @param chunkSize
    *   Number of items to return per HTTP request
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @param resourceVersion
    *   Control the returned resources' version.
    * @return
    *   A stream of resources
    */
  def getAll(
    chunkSize: Int = 10,
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None,
    resourceVersion: ListResourceVersion = ListResourceVersion.MostRecent
  ): Stream[K8sFailure, T] =
    asGenericResource.getAll(None, chunkSize, fieldSelector, labelSelector, resourceVersion)

  /** Watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * This function requires the user to control the starting resourceVersion and to restart the
    * watch stream when the server closes the connection.
    *
    * For a more convenient variant check [[watchForever]].
    *
    * @param resourceVersion
    *   Last known resource version
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watch(
    resourceVersion: Option[String],
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): Stream[K8sFailure, TypedWatchEvent[T]] =
    asGenericResource.watch(None, resourceVersion, fieldSelector, labelSelector)

  /** Infinite watch stream of resource change events of type
    * [[com.coralogix.zio.k8s.client.model.TypedWatchEvent]]
    *
    * The underlying implementation takes advantage of Kubernetes watch bookmarks.
    *
    * @param fieldSelector
    *   Constrain the returned items by field selectors. Not all fields are supported by the server.
    * @param labelSelector
    *   Constrain the returned items by label selectors.
    * @return
    *   A stream of watch events
    */
  def watchForever(
    fieldSelector: Option[FieldSelector] = None,
    labelSelector: Option[LabelSelector] = None
  ): ZStream[Clock, K8sFailure, TypedWatchEvent[T]] =
    asGenericResource.watchForever(None, fieldSelector, labelSelector)

  /** Get a resource by its name
    * @param name
    *   Name of the resource
    * @return
    *   Returns the current version of the resource
    */
  def get(name: String): IO[K8sFailure, T] =
    asGenericResource.get(name, None)

  /** Creates a new resource
    * @param newResource
    *   The new resource to define in the cluster.
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the created resource as it was returned from Kubernetes
    */
  def create(newResource: T, dryRun: Boolean = false): IO[K8sFailure, T] =
    asGenericResource.create(newResource, None, dryRun)

  /** Replaces an existing resource selected by its name
    * @param name
    *   Name of the resource
    * @param updatedResource
    *   The new value of the resource
    * @param dryRun
    *   If true, the request is sent to the server but it will not create the resource.
    * @return
    *   Returns the updated resource as it was returned from Kubernetes
    */
  def replace(
    name: String,
    updatedResource: T,
    dryRun: Boolean = false
  ): IO[K8sFailure, T] =
    asGenericResource.replace(name, updatedResource, None, dryRun)
}
