package edu.pjwstk.groups.usecase.updategroup;

import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.util.UUID;

public record UpdateGroupRequest(
        @NotNull(message = "Admin ID cannot be null")
        UUID adminId,

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
