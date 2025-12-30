package pl.gamilife.groupshop.application.getgroupshopdetails;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGroupShopDetailsCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID userId,

        @NotNull
        @PositiveOrZero
        Integer pageNumber,

        @NotNull
        @Positive
        Integer pageSize
) implements Command {


}
