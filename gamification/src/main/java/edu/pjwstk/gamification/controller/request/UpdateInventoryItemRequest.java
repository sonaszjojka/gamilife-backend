package edu.pjwstk.gamification.controller.request;

import jakarta.validation.constraints.Min;

public record UpdateInventoryItemRequest(
        Boolean equipped,

        @Min(0)
        Integer quantity
) {
}
