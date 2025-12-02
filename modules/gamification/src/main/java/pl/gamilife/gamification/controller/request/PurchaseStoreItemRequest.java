package pl.gamilife.gamification.controller.request;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record PurchaseStoreItemRequest(
        @NotNull
        UUID itemId
) {
}
