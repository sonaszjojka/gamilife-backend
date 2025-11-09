package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.core.UseCase;
import org.springframework.data.domain.Page;

public interface GetGroupsUseCase extends UseCase<GetGroupsCommand, Page<GetGroupsResult>> {
}
