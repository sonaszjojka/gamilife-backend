package pl.gamilife.group.usecase.findmembersbyidin;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.Collection;

public interface FindMembersByIdInUseCase extends UseCase<FindMembersByIdInCommand, Collection<FindMembersByIdInResult>> {
}
