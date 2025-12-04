package pl.gamilife.grouptask.usecase.getgrouptasks;

public record GetGroupTasksRequestFilter(
        Boolean isAccepted,
        Integer pageNumber,
        Integer pageSize
) {
}
