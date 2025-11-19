package edu.pjwstk.gamification.usecase.editinventoryitem;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditInventoryItemResult(
        UUID userInventoryId,
        Integer newQuantity,
        Integer newUserMoney,
        Boolean newIsEquipped
) {
}
