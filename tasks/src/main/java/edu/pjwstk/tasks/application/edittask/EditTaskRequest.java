package edu.pjwstk.tasks.application.edittask;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.annotation.Nulls;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

public record EditTaskRequest(
        @NotBlank(message = "Title cannot be blank")
        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @NotNull(message = "Start Time cannot be null")
        LocalDateTime startTime,

        LocalDateTime endTime,

        @NotNull(message = "Category Id cannot be null")
        Integer categoryId,

        @NotNull(message = "Difficulty Id cannot be null")
        Integer difficultyId,

        LocalDateTime completedAt,

        UUID habitTaskId,

        UUID previousTaskId,

        @Size(max = 200, message = "Description cannot exceed 200 characters")
        String description
) {

}