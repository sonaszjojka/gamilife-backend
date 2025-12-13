package pl.gamilife.groupshop.application.createownedgroupitem;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;


public record CreateOwnedGroupItemRequest(

        @NotNull
        UUID groupItemId,

        @NotNull
        Boolean isUsedUp

) {
}
