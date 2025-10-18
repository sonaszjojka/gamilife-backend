package edu.pjwstk.groups.usecase.creategroupmember;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record CreateGroupMemberRequest(

        @NotNull(message = "UserId cannot be null")
        UUID userId

) {
}
