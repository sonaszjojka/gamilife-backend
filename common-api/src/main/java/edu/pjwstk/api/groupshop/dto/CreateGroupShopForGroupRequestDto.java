package edu.pjwstk.api.groupshop.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.UUID;

public record CreateGroupShopForGroupRequestDto(

        @NotNull
        @Size(max =100)
        String name,

        @NotNull
        @Size(max=300)
        String description,

        @NotNull
        UUID groupId

) {
}
