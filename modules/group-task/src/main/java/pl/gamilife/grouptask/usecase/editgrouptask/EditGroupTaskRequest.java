package pl.gamilife.grouptask.usecase.editgrouptask;


import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

public record EditGroupTaskRequest(

        @Positive(message = "Reward must be positive")
        @Max(value = 9999, message = "Reward must not exceed 9999")
        Integer reward,

        Boolean isAccepted,

        @Size(max = 255, message = "Decline message must not exceed 255 characters")
        String declineMessage

) {
}
