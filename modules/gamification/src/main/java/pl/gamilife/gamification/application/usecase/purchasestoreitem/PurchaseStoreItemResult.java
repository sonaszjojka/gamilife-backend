package pl.gamilife.gamification.application.usecase.purchasestoreitem;

import java.util.UUID;

public record PurchaseStoreItemResult(
        UUID userInventoryItemId,
        UUID itemId,
        int quantity,
        int newUserMoney
) {
}
