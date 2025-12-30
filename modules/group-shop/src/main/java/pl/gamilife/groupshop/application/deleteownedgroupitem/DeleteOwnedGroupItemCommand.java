package pl.gamilife.groupshop.application.deleteownedgroupitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteOwnedGroupItemCommand(
        @NotNull
        UUID groupId,
        @NotNull
        UUID memberId,
        @NotNull
        UUID userId,
        @NotNull
        UUID ownedGroupItemId
) implements Command {
}
