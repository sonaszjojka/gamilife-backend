package edu.pjwstk.gamification.usecase.purchasestoreitem;

import java.util.UUID;

public record PurchaseStoreItemResult(UUID id, UUID itemId, Integer quantity) {
}
