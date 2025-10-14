package edu.pjwstk.groups.usecase.creategroup;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.util.UUID;

public record CreateGroupRequest(

        @NotNull(message = "Admin ID cannot be null")
        UUID adminId,

        @NotNull(message = "Group currency symbol cannot be null")
        Character groupCurrencySymbol,

        @NotNull(message = "Group type ID cannot be null")
        @Positive(message = "Group type ID must be positive")
        Integer groupTypeId,

        @NotNull(message = "Members limit cannot be null")
        @Min(value = 2, message = "Members limit must be at least 2")
        @Max(value = 100, message = "Members limit cannot exceed 100")
        Integer membersLimit
) {
}