package edu.pjwstk.common.emailSenderApi;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record MailDto(
        @Email
        @NotNull
        String toEmail,

        @NotBlank
        String subject,

        @NotBlank
        String content,

        @NotNull
        MailContentType mailContentType
) {
}
