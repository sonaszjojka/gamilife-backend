package pl.gamilife.groupshop.application.purchasegroupitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;


public record PurchaseGroupItemCommand(

        @NotNull
        UUID groupItemId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID memberId,
        @NotNull
        UUID currentUserId

) implements Command {
}
