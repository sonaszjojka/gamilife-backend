package pl.gamilife.group.usecase.getgrouptypes;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.repository.GroupTypeJpaRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class GetGroupTypeUseCaseImpl implements GetGroupTypesUseCase {

    private final GroupTypeJpaRepository groupTypeRepository;

    @Override
    public List<GetGroupTypesResult> execute(GetGroupTypesCommand cmd) {
        return groupTypeRepository.findAll().stream().map(groupType -> new GetGroupTypesResult(
                groupType.getId(),
                groupType.getTitle()
        )).toList();
    }
}
