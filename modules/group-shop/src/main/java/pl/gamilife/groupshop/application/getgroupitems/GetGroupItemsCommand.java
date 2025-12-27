package pl.gamilife.groupshop.application.getgroupitems;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGroupItemsCommand(

        @NotNull
        UUID groupId,

        @NotNull
        UUID shopId,

        @NotNull
        Boolean isActive,

        Integer pageNumber,

        Integer pageSize
) implements Command {


}
