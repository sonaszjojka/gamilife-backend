package pl.gamilife.groupshop.application.creategroupitem;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupItemInShopCommand(
        @NotNull
        @Size(min = 1, max = 30)
        String name,

        @NotNull
        @PositiveOrZero
        Integer price,

        @NotNull
        Boolean isActive,

        @NotNull
        UUID groupShopId,
        @NotNull
        UUID groupId,
        @NotNull
        UUID userId


) implements Command {
}
