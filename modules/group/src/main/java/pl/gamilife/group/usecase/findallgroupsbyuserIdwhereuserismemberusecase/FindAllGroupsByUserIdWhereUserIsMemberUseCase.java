package pl.gamilife.group.usecase.findallgroupsbyuserIdwhereuserismemberusecase;

import pl.gamilife.api.group.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface FindAllGroupsByUserIdWhereUserIsMemberUseCase
        extends UseCase<FindAllGroupsByUserIdWhereUserIsMemberCommand, FindAllGroupsByUserIdWhereUserIsMemberResult> {
}
