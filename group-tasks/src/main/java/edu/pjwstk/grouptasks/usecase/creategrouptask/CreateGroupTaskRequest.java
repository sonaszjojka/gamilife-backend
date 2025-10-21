package edu.pjwstk.grouptasks.usecase.creategrouptask;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Positive;



public record CreateGroupTaskRequest(


        @Positive(message = "Reward must be positive")
        @Max(value = 9999, message = "Reward must not exceed 9999")
        Integer reward

) {
}
