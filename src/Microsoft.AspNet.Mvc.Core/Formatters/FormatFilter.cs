// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNet.Mvc.ApiExplorer;
using Microsoft.AspNet.Mvc.Filters;
using Microsoft.AspNet.Mvc.Infrastructure;
using Microsoft.Extensions.OptionsModel;
using Microsoft.Net.Http.Headers;

namespace Microsoft.AspNet.Mvc.Formatters
{
    /// <summary>
    /// A filter which will use the format value in the route data or query string to set the content type on an 
    /// <see cref="ObjectResult" /> returned from an action.
    /// </summary>
    public class FormatFilter : IFormatFilter, IResourceFilter, IResultFilter
    {
        /// <summary>
        /// Initializes an instance of <see cref="FormatFilter"/>.
        /// </summary>
        /// <param name="options">The <see cref="IOptions{MvcOptions}"/></param>
        /// <param name="actionContextAccessor">The <see cref="IActionContextAccessor"/></param>
        public FormatFilter(IOptions<MvcOptions> options, IActionContextAccessor actionContextAccessor)
        {
            IsActive = true;
            Format = GetFormat(actionContextAccessor.ActionContext);

            if (string.IsNullOrEmpty(Format))
            {
                IsActive = false;
                return;
            }

            ContentType = options.Value.FormatterMappings.GetMediaTypeMappingForFormat(Format);
        }

        /// <summary>
        /// Format value in the current request. <c>null</c> if format not present in the current request.
        /// </summary>
        public string Format { get; }

        /// <summary>
        /// <see cref="MediaTypeHeaderValue"/> for the format value in the current request.
        /// </summary>
        public MediaTypeHeaderValue ContentType { get; }

        /// <summary>
        /// <c>true</c> if the current <see cref="FormatFilter"/> is active and will execute. 
        /// </summary>
        public bool IsActive { get; }

        /// <summary>
        /// As a <see cref="IResourceFilter"/>, this filter looks at the request and rejects it before going ahead if
        /// 1. The format in the request doesnt match any format in the map.
        /// 2. If there is a conflicting producesFilter.
        /// </summary>
        /// <param name="context">The <see cref="ResourceExecutingContext"/>.</param>
        public void OnResourceExecuting(ResourceExecutingContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }

            if (!IsActive)
            {
                return; // no format specified by user, so the filter is muted
            }

            if (ContentType == null)
            {
                // no contentType exists for the format, return 404
                context.Result = new HttpNotFoundResult();
                return;
            }

            var responseTypeFilters = context.Filters.OfType<IApiResponseMetadataProvider>();
            var contentTypes = new List<MediaTypeHeaderValue>();

            foreach (var filter in responseTypeFilters)
            {
                filter.SetContentTypes(contentTypes);
            }

            if (contentTypes.Count != 0)
            {
                // We need to check if the action can generate the content type the user asked for. If it cannot, exit
                // here with not found result. 
                if (!contentTypes.Any(c => ContentType.IsSubsetOf(c)))
                {
                    context.Result = new HttpNotFoundResult();
                }
            }
        }

        /// <inheritdoc />
        public void OnResourceExecuted(ResourceExecutedContext context)
        {
        }

        /// <summary>
        /// Sets a Content Type on an  <see cref="ObjectResult" />  using a format value from the request.
        /// </summary>
        /// <param name="context">The <see cref="ResultExecutingContext"/>.</param>
        public void OnResultExecuting(ResultExecutingContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }

            if (!IsActive)
            {
                return; // no format specified by user, so the filter is muted
            }

            var objectResult = context.Result as ObjectResult;
            if (objectResult != null)
            {
                objectResult.ContentTypes.Clear();
                objectResult.ContentTypes.Add(ContentType);
            }
        }

        /// <inheritdoc />
        public void OnResultExecuted(ResultExecutedContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }
        }

        private string GetFormat(ActionContext context)
        {
            object format = null;

            if (!context.RouteData.Values.TryGetValue("format", out format))
            {
                format = context.HttpContext.Request.Query["format"];
            }

            if (format != null)
            {
                return format.ToString();
            }

            return null;
        }
    }
}