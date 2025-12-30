package pl.gamilife.groupshop.application.creategroupshopforgroup;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupShopForGroupCommand(

        @NotNull
        UUID groupId,

        @NotNull
        @NotBlank
        String groupName

) implements Command {
}
