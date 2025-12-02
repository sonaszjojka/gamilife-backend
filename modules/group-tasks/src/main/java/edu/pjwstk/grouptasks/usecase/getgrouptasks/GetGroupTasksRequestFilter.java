package edu.pjwstk.grouptasks.usecase.getgrouptasks;

public record GetGroupTasksRequestFilter(
        Boolean isAccepted,
        Integer pageNumber,
        Integer pageSize
) {
}
