package edu.pjwstk.gamification.usecase.purchasestoreitem;

import java.util.UUID;

public record PurchaseStoreItemResult(
        UUID userInventoryId,
        UUID itemId,
        int quantity,
        int newUserMoney
) {
}
