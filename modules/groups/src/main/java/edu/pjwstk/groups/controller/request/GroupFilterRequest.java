package edu.pjwstk.groups.controller.request;

import jakarta.validation.constraints.Min;

public record GroupFilterRequest(
        String joinCode,
        Integer type,
        String name,

        @Min(value = 0, message = "Page number must be >= 0")
        Integer page,

        @Min(value = 1, message = "Page size must be >= 1")
        Integer size
) {
}