package edu.pjwstk.groups.controller.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record CreateChatMessageRequest(
        @NotBlank(message = "Content cannot be blank!")
        String content,
        @NotNull(message = "Is important flag cannot be null!")
        Boolean isImportant
) {
}
