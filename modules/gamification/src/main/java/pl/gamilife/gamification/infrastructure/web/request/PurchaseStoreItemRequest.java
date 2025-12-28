package pl.gamilife.gamification.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record PurchaseStoreItemRequest(
        @NotNull
        UUID itemId
) {
}
