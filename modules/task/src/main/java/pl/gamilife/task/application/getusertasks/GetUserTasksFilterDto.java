package pl.gamilife.task.application.getusertasks;

public record GetUserTasksFilterDto(
        Integer categoryId,
        Integer difficultyId,
        Boolean isCompleted,
        Boolean isGroupTask,
        Integer pageNumber,
        Integer pageSize
) {
}
