package pl.gamilife.grouptask.usecase.getgrouptasks;

public record GetGroupTasksRequestFilter(
        Boolean isAccepted,
        Boolean isDeclined,
        Integer pageNumber,
        Integer pageSize
) {
}
