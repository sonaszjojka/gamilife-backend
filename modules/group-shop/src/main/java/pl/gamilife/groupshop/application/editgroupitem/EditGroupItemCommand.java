package pl.gamilife.groupshop.application.editgroupitem;


import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupItemCommand(

        @Size(min = 1, max = 30)
        String name,

        @PositiveOrZero
        Integer price,

        Boolean isActive,

        UUID groupItemId,
        UUID groupId,
        UUID currentUserId




) implements Command {
}
