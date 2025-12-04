package pl.gamilife.groupshop.usecase.createownedgroupitem;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;


public record CreateOwnedGroupItemRequest(

        @NotNull
        UUID groupItemId,

        @NotNull
        Boolean isUsedUp

) {
}
