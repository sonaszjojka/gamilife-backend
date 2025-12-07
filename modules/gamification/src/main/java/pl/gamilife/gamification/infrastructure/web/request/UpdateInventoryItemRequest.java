package pl.gamilife.gamification.infrastructure.web.request;

import jakarta.validation.constraints.Min;

public record UpdateInventoryItemRequest(
        @Min(1)
        Integer subtractQuantityBy,
        Boolean isEquipped
) {
}
