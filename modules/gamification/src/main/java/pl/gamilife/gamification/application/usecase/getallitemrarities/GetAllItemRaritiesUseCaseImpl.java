package pl.gamilife.gamification.application.usecase.getallitemrarities;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.port.repository.RarityRepository;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetAllItemRaritiesUseCaseImpl implements GetAllItemRaritiesUseCase {

    private final RarityRepository rarityRepository;

    @Override
    public GetAllItemRaritiesResult execute(GetAllItemRaritiesCommand cmd) {
        return new GetAllItemRaritiesResult(
                rarityRepository.findAll().stream()
                        .map(rarity -> new GetAllItemRaritiesResult.ItemRarityDTO(
                                rarity.getId(),
                                rarity.getName()
                        ))
                        .toList()
        );
    }
}
