package edu.pjwstk.tasks.application.getusertasks;

public record GetUserTasksFilterDto(
        Integer categoryId,
        Integer difficultyId,
        Boolean isCompleted,
        Boolean isGroupTask,
        Integer pageNumber,
        Integer pageSize
) {
}
