package pl.gamilife.groupshop.application.editownedgroupitem;

import jakarta.validation.constraints.NotNull;

public record EditOwnedGroupItemRequest(
        @NotNull
        Boolean isUsedUp
) {
}
