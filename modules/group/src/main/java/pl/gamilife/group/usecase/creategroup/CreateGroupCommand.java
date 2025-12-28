package pl.gamilife.group.usecase.creategroup;

import jakarta.validation.constraints.*;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record CreateGroupCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        @NotBlank
        @Size(max = 50)
        String groupName,

        @NotNull
        Character groupCurrencySymbol,

        @NotNull
        Integer groupTypeId,

        @NotNull()
        @Min(value = 2)
        @Max(value = 100)
        Integer membersLimit
) implements Command {
}
