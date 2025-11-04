package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.groups.shared.GroupTypeEnum;
import jakarta.validation.constraints.*;

import java.util.UUID;

public record UpdateGroupRequest(
        @NotNull(message = "Admin ID cannot be null")
        UUID adminId,

        @NotBlank(message = "Group name cannot be blank")
        @Size(max = 50, message = "Group name cannot exceed 50 characters")
        String groupName,

        @NotNull(message = "Group currency symbol cannot be null")
        Character groupCurrencySymbol,

        @NotNull(message = "Group type cannot be null")
        GroupTypeEnum groupType,

        @NotNull(message = "Members limit cannot be null")
        @Min(value = 2, message = "Members limit must be at least 2")
        @Max(value = 100, message = "Members limit cannot exceed 100")
        Integer membersLimit
) {
}
