package pl.gamilife.gamification.application.usecase.editinventoryitem;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditInventoryItemResult(
        UUID userInventoryItemId,
        Integer newQuantity,
        Integer newUserMoney,
        Boolean newIsEquipped
) {
}
