package edu.pjwstk.groups.usecase.findallgroupsbyuserIdwhereuserismemberusecase;

import edu.pjwstk.api.groups.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface FindAllGroupsByUserIdWhereUserIsMemberUseCase
        extends UseCase<FindAllGroupsByUserIdWhereUserIsMemberCommand, FindAllGroupsByUserIdWhereUserIsMemberResult> {
}
