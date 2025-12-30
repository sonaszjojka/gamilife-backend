package pl.gamilife.groupshop.application.getgroupshopitems;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGroupShopItemsCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID userId,

        Boolean isActive,

        @NotNull
        @PositiveOrZero
        Integer page,

        @NotNull
        @Positive
        int size
) implements Command {
}
