package edu.pjwstk.groupshop.usecase.editownedgroupitem;

import jakarta.validation.constraints.NotNull;

public record EditOwnedGroupItemRequest(
        @NotNull
        Boolean isUsedUp
) {
}
